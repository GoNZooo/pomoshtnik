{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Discord (RunDiscordOpts (..))
import qualified Discord
import Discord.Types (CreateEmbed (..), CreateEmbedImage (..), EmbedField (..), Event (..), Message (..), User (..))
import DiscordSandbox.Discord (onEvent, onStart, replyTo)
import qualified DiscordSandbox.TMDB as TMDB
import DiscordSandbox.TMDB.Types
  ( CastEntry (..),
    Credits (..),
    KnownFor (..),
    KnownForMovieData (..),
    KnownForShowData (..),
    Movie (..),
    MovieCandidate (..),
    Person (..),
    PersonCandidate (..),
  )
import DiscordSandbox.Web (WebBase (..))
import Import
import qualified Network.Wai.Handler.Warp as Warp
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified System.Environment as Environment
import qualified Yesod.Core as Yesod

run :: RIO App ()
run = do
  token <- liftIO $ Environment.getEnv "DISCORD_API_TOKEN" >>= \t -> pure $ "Bot " <> Text.pack t
  logFunction <- asks appLogFunc
  eventQueue <- asks appDiscordEvents
  handleReference <- asks appDiscordHandle
  appState <- ask

  let runCommandHandler = do
        forever $ do
          event <- atomically $ decodeCommand <$> readTQueue eventQueue
          runRIO appState $ maybe mempty handleCommand event
      runDiscordInputThread = do
        liftIO $
          Discord.runDiscord
            Discord.def
              { discordToken = token,
                discordOnStart = onStart handleReference $ runRIO logFunction $ discordLog "Started reading messages",
                discordOnEvent = onEvent eventQueue
              }
  (discordResult, ()) <-
    liftIO $
      concurrently runDiscordInputThread $
        concurrently_ runCommandHandler $ Warp.run 4000 =<< Yesod.toWaiApp (WebBase appState)
  logErrorS "Discord" $ display discordResult

decodeCommand :: Event -> Maybe IncomingCommand
decodeCommand (MessageCreate Message {messageText = text, messageAuthor = author, messageChannel = channelId'})
  | text == "!generate-token" =
    Just $ IncomingCommand {channelId = channelId', user = author, command = GenerateToken}
  | text == "!authenticated" =
    Just $ IncomingCommand {channelId = channelId', user = author, command = AuthenticatedUsers}
  | "!login " `Text.isPrefixOf` text =
    case Text.split (== ' ') text of
      _ : token : _ ->
        case UUID.fromText token of
          Just uuid -> Just $ IncomingCommand {channelId = channelId', user = author, command = Login uuid}
          Nothing -> Nothing
      _ -> Nothing
  | "!movie " `Text.isPrefixOf` text =
    case Text.split (== ' ') text of
      _ : rest ->
        Just $
          IncomingCommand
            { channelId = channelId',
              user = author,
              command =
                SearchMovie $ MovieTitle $ Text.intercalate (" " :: Text) rest
            }
      _ -> Nothing
  | "!movie-candidates " `Text.isPrefixOf` text =
    case Text.split (== ' ') text of
      _ : rest ->
        Just $
          IncomingCommand
            { channelId = channelId',
              user = author,
              command =
                SearchMovieCandidates $ MovieTitle $ Text.intercalate (" " :: Text) rest
            }
      _ -> Nothing
  | "!movie-by-id " `Text.isPrefixOf` text =
    case Text.split (== ' ') text of
      _ : rest ->
        let maybeId = fmap MovieId $ readMaybe $ Text.unpack $ Text.concat rest
         in case maybeId of
              Just movieId ->
                Just $ IncomingCommand {channelId = channelId', user = author, command = GetMovie movieId}
              Nothing -> Nothing
      _ -> Nothing
  | "!person " `Text.isPrefixOf` text =
    case Text.split (== ' ') text of
      _ : rest ->
        Just $
          IncomingCommand
            { channelId = channelId',
              user = author,
              command =
                SearchPerson $ PersonName $ Text.intercalate (" " :: Text) rest
            }
      _ -> Nothing
  | otherwise = Nothing
decodeCommand _ = Nothing

handleCommand ::
  ( MonadReader env m,
    MonadIO m,
    HasLogFunc env,
    HasActiveTokens env,
    HasAuthenticatedUsers env,
    HasDiscordHandle env,
    HasTMDBAPIKey env,
    HasTLSConnectionManager env,
    HasTMDBImageConfigurationData env
  ) =>
  IncomingCommand ->
  m ()
handleCommand IncomingCommand {user = user', command = GenerateToken} = do
  newToken <- addNewToken user'
  discordLog $ "Added token '" <> tshow newToken <> "' for user with ID '" <> userName user' <> "'"
handleCommand IncomingCommand {channelId = channelId', user = user', command = Login suppliedToken} = do
  whenM (authenticateUser user' suppliedToken) $ do
    replyTo channelId' user' (Just "You have been authenticated.") Nothing
handleCommand IncomingCommand {channelId = channelId', user = user', command = AuthenticatedUsers} = do
  usersReference <- view authenticatedUsersL
  whenM (userIsAuthenticated user') $ do
    authenticatedUsers <- readTVarIO usersReference
    let usersString =
          Text.intercalate
            "\n"
            (Set.elems $ Set.map (\u -> "- " <> userName u <> "#" <> userDiscrim u) authenticatedUsers)
        messageEmbed =
          Discord.def
            { createEmbedFields =
                [ EmbedField
                    { embedFieldName = "Authenticated Users",
                      embedFieldValue = usersString,
                      embedFieldInline = Nothing
                    }
                ]
            }
    replyTo channelId' user' Nothing (Just messageEmbed)
handleCommand IncomingCommand {channelId = channelId', user = user', command = SearchMovie movieTitle} = do
  movieResult <- TMDB.searchMovieM movieTitle
  case movieResult of
    Right movie -> do
      ImageConfigurationData {secureBaseUrl = imageBaseUrl} <- view tmdbImageConfigurationDataL
      let embed = movieEmbed imageBaseUrl PosterW780 movie
      replyTo channelId' user' Nothing embed
    Left error' -> replyTo channelId' user' (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId = channelId', user = user', command = SearchMovieCandidates movieTitle} = do
  movieCandidatesResult <- TMDB.searchMovieCandidatesM movieTitle
  case movieCandidatesResult of
    Right movieCandidates -> do
      let embed = Just $ movieCandidatesEmbed movieCandidates
      replyTo channelId' user' Nothing embed
    Left error' -> replyTo channelId' user' (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId = channelId', user = user', command = GetMovie movieId} = do
  movieCandidatesResult <- TMDB.getMovieM movieId
  case movieCandidatesResult of
    Right movie -> do
      ImageConfigurationData {secureBaseUrl = imageBaseUrl} <- view tmdbImageConfigurationDataL
      let embed = movieEmbed imageBaseUrl PosterW780 movie
      replyTo channelId' user' Nothing embed
    Left error' -> replyTo channelId' user' (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId = channelId', user = user', command = SearchPerson personName} = do
  personCandidateResult <- TMDB.searchPersonM personName
  case personCandidateResult of
    Right personCandidate@PersonCandidate {id = personId} -> do
      personResult <- TMDB.getPersonM personId
      case personResult of
        Right person -> do
          ImageConfigurationData {secureBaseUrl = imageBaseUrl} <- view tmdbImageConfigurationDataL
          let embed = personEmbed imageBaseUrl ProfileOriginal personCandidate person
          replyTo channelId' user' Nothing embed
        Left error' -> replyTo channelId' user' (Just $ fromString error') Nothing
    Left error' -> replyTo channelId' user' (Just $ fromString error') Nothing

movieEmbed :: Text -> PosterSize -> Movie -> Maybe CreateEmbed
movieEmbed
  imageBaseUrl
  posterSize
  Movie
    { title = Just (MovieTitle title'),
      overview = overview',
      voteAverage = rating,
      releaseDate = releaseDate',
      imdbId = imdbId',
      posterPath = posterPath',
      credits = credits'
    } =
    let fields =
          [ EmbedField {embedFieldName = "Description", embedFieldValue = overview', embedFieldInline = Nothing}
          ]
            <> maybe [] (\castEntries -> [castField castEntries]) (credits' & cast)
        castField castEntries =
          EmbedField
            { embedFieldName = "Cast",
              embedFieldValue = castText castEntries,
              embedFieldInline = Just True
            }
        castText :: [CastEntry] -> Text
        castText castEntries =
          take maxCastEntries castEntries
            & map
              (\CastEntry {name = name', character = character'} -> mconcat ["**", name', "** as ", character'])
            & Text.intercalate "\n"
        titleText = mconcat [title', " (", tshow rating, maybe "" (", " <>) releaseDate', ")"]
        embedImage = fmap (CreateEmbedImageUrl . posterUrl imageBaseUrl posterSize) posterPath'
     in pure $
          Discord.def
            { createEmbedTitle = titleText,
              createEmbedFields = fields,
              createEmbedUrl = TMDB.imdbMovieUrl imdbId',
              createEmbedImage = embedImage
            }
movieEmbed _ _ _ = Nothing

movieCandidatesEmbed :: [MovieCandidate] -> CreateEmbed
movieCandidatesEmbed candidates =
  let fields =
        take 10 candidates
          & fmap
            ( \MovieCandidate
                 { title = title',
                   id = id',
                   overview = overview',
                   voteAverage = voteAverage',
                   releaseDate = releaseDate'
                 } ->
                  EmbedField
                    { embedFieldName =
                        mconcat
                          [maybe "" unMovieTitle title', "(", tshow $ unMovieId id', ", ", tshow voteAverage']
                          <> maybe "" (", " <>) releaseDate'
                          <> ")",
                      embedFieldValue = overview',
                      embedFieldInline = Nothing
                    }
            )
   in Discord.def
        { createEmbedFields = fields
        }

personEmbed :: Text -> ProfileSize -> PersonCandidate -> Person -> Maybe CreateEmbed
personEmbed
  imageBaseUrl
  profileSize
  PersonCandidate {knownFor = knownFor'}
  Person
    { name = PersonName name',
      popularity = popularity',
      imdbId = imdbId',
      profilePath = profilePath',
      knownForDepartment = knownForDepartment'
    } =
    let fields = movieFields knownFor'
        movieFields :: [KnownFor] -> [EmbedField]
        movieFields = fmap knownForToField
        knownForToField :: KnownFor -> EmbedField
        knownForToField
          ( KnownForMovie
              KnownForMovieData
                { title = title',
                  voteAverage = voteAverage',
                  releaseDate = releaseDate',
                  overview = overview'
                }
            ) =
            EmbedField
              { embedFieldName =
                  mconcat
                    [ maybe "" identity releaseDate',
                      ": ",
                      maybe "" identity title',
                      " (",
                      tshow voteAverage',
                      ")"
                    ],
                embedFieldValue = overview',
                embedFieldInline = Nothing
              }
        knownForToField
          ( KnownForShow
              KnownForShowData
                { firstAirDate = firstAirDate',
                  overview = overview',
                  voteAverage = voteAverage',
                  name = showName
                }
            ) =
            EmbedField
              { embedFieldName =
                  mconcat
                    [ maybe "" identity firstAirDate',
                      ": ",
                      maybe "" identity showName,
                      " (",
                      tshow voteAverage',
                      ")"
                    ],
                embedFieldValue = overview',
                embedFieldInline = Nothing
              }
        embedImage = fmap (CreateEmbedImageUrl . profileUrl imageBaseUrl profileSize) profilePath'
     in pure $
          Discord.def
            { createEmbedTitle = name',
              createEmbedFields = fields,
              createEmbedUrl = TMDB.imdbPersonUrl imdbId',
              createEmbedImage = embedImage,
              createEmbedFooterText =
                "Known for: " <> knownForDepartment' <> "    " <> "Popularity: " <> tshow popularity'
            }

posterUrl :: Text -> PosterSize -> Text -> Text
posterUrl imageBaseUrl posterSize posterPath' =
  let toUrlFragment posterSize' = removePrefix "Poster" (show posterSize') & camelCase & fromString
   in mconcat [imageBaseUrl, toUrlFragment posterSize, posterPath']

profileUrl :: Text -> ProfileSize -> Text -> Text
profileUrl imageBaseUrl profileSize profilePath' =
  let toUrlFragment posterSize' = removePrefix "Profile" (show posterSize') & camelCase & fromString
   in mconcat [imageBaseUrl, toUrlFragment profileSize, profilePath']

addNewToken :: (MonadReader env m, MonadIO m, HasActiveTokens env) => User -> m UUID
addNewToken user' = do
  tokensReference <- view activeTokensL
  newToken <- liftIO UUID.nextRandom
  atomically $ modifyTVar' tokensReference $ Map.insert user' newToken
  pure newToken

authenticateUser ::
  (MonadReader env m, MonadIO m, HasActiveTokens env, HasAuthenticatedUsers env) =>
  User ->
  UUID ->
  m Bool
authenticateUser user' token = do
  tokensReference <- view activeTokensL
  usersReference <- view authenticatedUsersL
  atomically $ do
    tokens <- readTVar tokensReference
    if Just token /= Map.lookup user' tokens
      then pure False
      else do
        modifyTVar usersReference $ Set.insert user'
        pure True

userIsAuthenticated :: (MonadReader env m, MonadIO m, HasAuthenticatedUsers env) => User -> m Bool
userIsAuthenticated user' = do
  usersReference <- view authenticatedUsersL
  users <- readTVarIO usersReference
  pure $ user' `Set.member` users

discordLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
discordLog message = logInfoS "Discord" $ display message

maxCastEntries :: Int
maxCastEntries = 20
