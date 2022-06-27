{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run,
  )
where

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.Persist (Entity (..))
import Database.Persist.Sql (BackendKey (SqlBackendKey))
import Discord (RunDiscordOpts (..))
import qualified Discord
import Discord.Types
  ( ChannelId,
    CreateEmbed (..),
    CreateEmbedImage (..),
    EmbedField (..),
    Event (..),
    Message (..),
    User (..),
  )
import Import
import qualified Pomoshtnik.Database as Database
import Pomoshtnik.Discord
  ( argumentsAsText,
    onEvent,
    onStart,
    oneArgument,
    oneArgumentAndText,
    replyTo,
    twoArguments,
  )
import Pomoshtnik.GitHub
import qualified Pomoshtnik.SevernataZvezda as SevernataZvezda
import qualified Pomoshtnik.TMDB as TMDB
import Pomoshtnik.TMDB.Types
  ( CastEntry (..),
    Credits (..),
    Episode (..),
    ExternalIds (..),
    KnownFor (..),
    KnownForMovieData (..),
    KnownForShowData (..),
    Movie (..),
    MovieCandidate (..),
    Person (..),
    PersonCandidate (..),
    ShowCandidate (..),
    TVShow (..),
  )
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified System.Environment as Environment

run :: RIO App ()
run = do
  applicationState@App {appLogFunc, appDiscordEvents, appDiscordHandle} <- ask
  discordToken <-
    liftIO $
      Environment.getEnv "DISCORD_API_TOKEN" >>= \t ->
        pure $
          "Bot " <> Text.pack t
  let runCommandHandler =
        forever $ do
          maybeEvent <- atomically $ decodeCommand <$> readTQueue appDiscordEvents
          for_ maybeEvent (handleCommand >>> runRIO applicationState)
      discordOnStart =
        onStart appDiscordHandle $ runRIO appLogFunc $ discordLog "Started reading messages"
      discordOnEvent = onEvent appDiscordEvents
      runDiscordInputThread =
        liftIO $ Discord.runDiscord Discord.def {discordToken, discordOnStart, discordOnEvent}
  (discordResult, ()) <-
    liftIO $
      concurrently runDiscordInputThread runCommandHandler
  logErrorS "Discord" $ display discordResult

decodeCommand :: Event -> Maybe IncomingCommand
decodeCommand (MessageCreate message@Message {messageText, messageAuthor, messageChannel})
  | messageText == "!generate-token" =
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command = GenerateToken}
  | messageText == "!authenticated" =
    let command = AuthenticatedUsers
     in pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!login " = do
    command <- Login <$> (oneArgument message >>= UUID.fromText)
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!movie " = do
    command <- (MovieTitle >>> SearchMovie) <$> argumentsAsText message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!movie-candidates " = do
    command <- (MovieTitle >>> SearchMovieCandidates) <$> argumentsAsText message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!movie-by-id " = do
    idText <- argumentsAsText message
    command <- (MovieId >>> GetMovie) <$> (idText & Text.unpack & readMaybe)
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!show " = do
    command <- (ShowTitle >>> SearchShow) <$> argumentsAsText message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!show-candidates " = do
    command <- (ShowTitle >>> SearchShowCandidates) <$> argumentsAsText message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!show-by-id " = do
    command <- (ShowId >>> GetShow) <$> (oneArgument message >>= (Text.unpack >>> readMaybe))
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!person " = do
    command <- (PersonName >>> SearchPerson) <$> argumentsAsText message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!add-note " = do
    (title, body) <- oneArgumentAndText message
    let command = NoteCommand $ AddNote title body
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!remove-note " = do
    command <- (RemoveNoteByTitle >>> NoteCommand) <$> oneArgument message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!remove-all-notes " = do
    command <- (RemoveNoteByFullTextSearch >>> NoteCommand) <$> argumentsAsText message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!update-note " = do
    (title, body) <- oneArgumentAndText message
    let command = NoteCommand $ UpdateNote title body
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!search-note " = do
    command <- (FullTextSearchNote >>> NoteCommand) <$> argumentsAsText message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!add-to-note " = do
    (title, addition) <- oneArgumentAndText message
    let command = NoteCommand $ AddToNote title addition
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!authenticate-external " = do
    (username, challengeText) <- twoArguments message
    let command =
          AuthenticateExternal (AuthenticationUsername username) $
            AuthenticationChallenge challengeText
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!gh-repos " = do
    command <- (Username >>> GitHubGetUserRepositories >>> GitHubCommand) <$> oneArgument message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!gh-repo " = do
    (username, repository) <- twoArguments message
    let command = GitHubCommand $ GitHubGetRepository (Username username) (RepositoryName repository)
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | messageText `startsWith` "!gh-user " = do
    command <- (Username >>> GitHubGetUser >>> GitHubCommand) <$> oneArgument message
    pure IncomingCommand {channelId = messageChannel, user = messageAuthor, command}
  | otherwise = Nothing
decodeCommand _ = Nothing

handleCommand ::
  ( MonadReader env m,
    MonadUnliftIO m,
    GitHub m,
    HasLogFunc env,
    HasActiveTokens env,
    HasAuthenticatedUsers env,
    HasDiscordHandle env,
    HasTMDBAPIKey env,
    HasTLSConnectionManager env,
    HasTMDBImageConfigurationData env,
    HasSqlPool env,
    HasExternalAuthenticationUrl env,
    HasExternalAuthenticationToken env
  ) =>
  IncomingCommand ->
  m ()
handleCommand IncomingCommand {user, command = GenerateToken} = do
  newToken <- addNewToken user
  discordLog $ "Added token '" <> tshow newToken <> "' for user with ID '" <> userName user <> "'"
handleCommand IncomingCommand {channelId, user, command = Login suppliedToken} =
  whenM (authenticateUser user suppliedToken) $
    replyTo channelId user (Just "You have been authenticated.") Nothing
handleCommand IncomingCommand {channelId, user, command = AuthenticatedUsers} = do
  usersReference <- view authenticatedUsersL
  whenM (userIsAuthenticated user) $ do
    authenticatedUsers <- readTVarIO usersReference
    let usersString =
          Text.unlines
            ( Set.elems $
                Set.map (\u -> "- " <> userName u <> "#" <> userDiscrim u) authenticatedUsers
            )
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
    replyTo channelId user Nothing (Just messageEmbed)
handleCommand IncomingCommand {channelId, user, command = SearchMovie movieTitle} = do
  movieResult <- TMDB.searchMovieM movieTitle
  case movieResult of
    Right movie -> do
      imageBaseUrl <- secureBaseUrl <$> view tmdbImageConfigurationDataL
      let embed = movieEmbed imageBaseUrl PosterW780 movie
      replyTo channelId user Nothing embed
    Left error' -> replyTo channelId user (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId, user, command = SearchMovieCandidates movieTitle} = do
  movieCandidatesResult <- TMDB.searchMovieCandidatesM movieTitle
  case movieCandidatesResult of
    Right movieCandidates -> do
      let embed = Just $ movieCandidatesEmbed movieCandidates
      replyTo channelId user Nothing embed
    Left error' -> replyTo channelId user (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId, user, command = GetMovie movieId} = do
  movieCandidatesResult <- TMDB.getMovieM movieId
  case movieCandidatesResult of
    Right movie -> do
      ImageConfigurationData {secureBaseUrl = imageBaseUrl} <- view tmdbImageConfigurationDataL
      let embed = movieEmbed imageBaseUrl PosterW780 movie
      replyTo channelId user Nothing embed
    Left error' -> replyTo channelId user (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId, user, command = SearchShow showTitle} = do
  movieResult <- TMDB.searchShowM showTitle
  case movieResult of
    Right movie -> do
      imageBaseUrl <- secureBaseUrl <$> view tmdbImageConfigurationDataL
      let embed = showEmbed imageBaseUrl PosterW780 movie
      replyTo channelId user Nothing embed
    Left error' -> replyTo channelId user (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId, user, command = SearchShowCandidates showTitle} = do
  showCandidatesResult <- TMDB.searchShowCandidatesM showTitle
  case showCandidatesResult of
    Right showCandidates -> do
      let embed = Just $ showCandidatesEmbed showCandidates
      replyTo channelId user Nothing embed
    Left error' -> replyTo channelId user (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId, user, command = GetShow showId} = do
  movieCandidatesResult <- TMDB.getShowM showId
  case movieCandidatesResult of
    Right movie -> do
      ImageConfigurationData {secureBaseUrl = imageBaseUrl} <- view tmdbImageConfigurationDataL
      let embed = showEmbed imageBaseUrl PosterW780 movie
      replyTo channelId user Nothing embed
    Left error' -> replyTo channelId user (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId, user, command = SearchPerson personName} = do
  personCandidateResult <- TMDB.searchPersonM personName
  case personCandidateResult of
    Right personCandidate@PersonCandidate {id = personId} -> do
      personResult <- TMDB.getPersonM personId
      case personResult of
        Right person -> do
          ImageConfigurationData {secureBaseUrl = imageBaseUrl} <- view tmdbImageConfigurationDataL
          let embed = personEmbed imageBaseUrl ProfileOriginal personCandidate person
          replyTo channelId user Nothing embed
        Left error' -> replyTo channelId user (Just $ fromString error') Nothing
    Left error' -> replyTo channelId user (Just $ fromString error') Nothing
handleCommand IncomingCommand {channelId, user, command = AuthenticateExternal username challenge} = do
  authenticationResult <-
    SevernataZvezda.authenticateChallengeM username challenge (Username $ constructUsername user)
  case authenticationResult of
    Just () -> replyTo channelId user (Just "Server responded successfully.") Nothing
    Nothing -> replyTo channelId user (Just "Server responded with error.") Nothing
handleCommand IncomingCommand {channelId, user, command = GitHubCommand gitHubCommand} =
  handleGitHubCommand channelId user gitHubCommand
handleCommand IncomingCommand {channelId, user, command = NoteCommand noteCommand} = do
  maybeUser <- Database.getOrCreateUserM $ Username $ constructUsername user
  handleNoteCommand maybeUser channelId user noteCommand

handleNoteCommand ::
  ( MonadReader env m,
    MonadUnliftIO m,
    HasDiscordHandle env,
    HasSqlPool env
  ) =>
  Maybe (Entity Database.User) ->
  ChannelId ->
  User ->
  NoteCommandType ->
  m ()
handleNoteCommand Nothing _channelId _user _noteCommand = pure ()
handleNoteCommand (Just (Entity userId _)) channelId user (AddNote noteTitle noteBody) = do
  maybeNoteId <- Database.addNoteM Database.Note {noteTitle, noteBody, noteUserId = userId}
  case maybeNoteId of
    Just (Database.NoteKey (SqlBackendKey noteId)) ->
      replyTo channelId user (Just $ "Note added with ID: " <> tshow noteId) Nothing
    Nothing -> do
      maybeAddedToNote <- Database.addToNoteM userId noteTitle noteBody
      case maybeAddedToNote of
        Just () ->
          replyTo
            channelId
            user
            (Just $ "Added to note with title: '" <> noteTitle <> "'")
            Nothing
        Nothing ->
          replyTo
            channelId
            user
            (Just "Unexpected error, note did not exist when still not found")
            Nothing
handleNoteCommand (Just (Entity userId _)) channelId user (AddToNote noteTitle noteBody) = do
  maybeSuccess <- Database.addToNoteM userId noteTitle noteBody
  case maybeSuccess of
    Just () ->
      replyTo channelId user (Just "Note added to") Nothing
    Nothing ->
      replyTo
        channelId
        user
        (Just $ "Unable to add note; title already exists: '" <> noteTitle <> "'")
        Nothing
handleNoteCommand (Just (Entity userId _)) channelId user (RemoveNoteByTitle title) = do
  Database.removeNoteByTitleM userId title
  replyTo channelId user (Just "Note removal completed.") Nothing
handleNoteCommand (Just (Entity userId _)) channelId user (RemoveNoteByFullTextSearch text) = do
  Database.removeNoteByFullTextSearchM userId text
  replyTo channelId user (Just "Note removals completed.") Nothing
handleNoteCommand (Just (Entity userId _)) channelId user (UpdateNote noteTitle noteBody) = do
  let note' = Database.Note {noteTitle, noteBody, noteUserId = userId}
  Database.updateNoteM note'
  replyTo channelId user (Just "Note updated.") Nothing
handleNoteCommand (Just (Entity userId _)) channelId user (FullTextSearchNote searchText) = do
  notes <- Database.findNotesByTextM userId searchText
  let embed = notesEmbed notes
  replyTo channelId user Nothing (Just embed)

handleGitHubCommand ::
  ( MonadReader env m,
    MonadUnliftIO m,
    GitHub m,
    HasDiscordHandle env
  ) =>
  ChannelId ->
  User ->
  GitHubCommandType ->
  m ()
handleGitHubCommand channelId user (GitHubGetUserRepositories username) = do
  repositories <- getGitHubUserRepositories username
  replyTo channelId user Nothing $ Just (repositoriesEmbed username repositories)
handleGitHubCommand channelId user (GitHubGetUser username) = do
  gitHubUser <- getGitHubUser username
  replyTo channelId user Nothing $ Just (gitHubUserEmbed gitHubUser)
handleGitHubCommand channelId user (GitHubGetRepository username repository) = do
  gitHubRepository <- getGitHubRepository username repository
  replyTo channelId user Nothing $ Just (repositoryEmbed gitHubRepository)

getLanguageCounts :: [GitHubRepository] -> Map Text Int
getLanguageCounts =
  foldr (\r m -> Map.insertWith (+) (fromMaybe "N/A" $ r ^. gitHubRepositoryLanguage) 1 m) Map.empty

repositoriesEmbed :: Username -> [GitHubRepository] -> CreateEmbed
repositoriesEmbed username repositories = do
  let embedFields =
        repositories
          & getLanguageCounts
          & Map.toList
          & List.sortBy (\a b -> compare (snd b) (snd a))
          & List.take 10
          & map
            ( \(language, count) ->
                EmbedField
                  { embedFieldName = language,
                    embedFieldValue = tshow count,
                    embedFieldInline = Nothing
                  }
            )
      description =
        mconcat
          [ "Repository statistics for [",
            unUsername username,
            "](https://github.com/",
            unUsername username,
            ")"
          ]
  Discord.def
    { createEmbedTitle = "GitHub repositories",
      createEmbedDescription = description,
      createEmbedFields = embedFields
    }

gitHubUserEmbed :: GitHubUser -> CreateEmbed
gitHubUserEmbed user =
  Discord.def
    { createEmbedTitle = user ^. gitHubUserLogin,
      createEmbedDescription = fromMaybe "N/A" $ user ^. gitHubUserBio,
      createEmbedFields = []
    }

repositoryEmbed :: GitHubRepository -> CreateEmbed
repositoryEmbed repository =
  Discord.def
    { createEmbedTitle = repository ^. gitHubRepositoryFullName,
      createEmbedDescription = fromMaybe "N/A" $ repository ^. gitHubRepositoryDescription,
      createEmbedFields = []
    }

constructUsername :: User -> Text
constructUsername user = mconcat [userName user, "#", userDiscrim user]

notesEmbed :: [Entity Database.Note] -> CreateEmbed
notesEmbed notes =
  let createEmbedFields =
        ( \(Entity (Database.NoteKey (SqlBackendKey noteId)) Database.Note {noteTitle, noteBody}) ->
            EmbedField
              { embedFieldName = noteTitle <> " [" <> tshow noteId <> "]",
                embedFieldValue = noteBody,
                embedFieldInline = Nothing
              }
        )
          <$> notes
   in Discord.def {createEmbedFields}

movieEmbed :: Text -> PosterSize -> Movie -> Maybe CreateEmbed
movieEmbed
  imageBaseUrl
  posterSize
  Movie
    { title = Just (MovieTitle title),
      overview,
      voteAverage,
      releaseDate,
      imdbId,
      posterPath,
      credits = Credits {cast}
    } =
    let createEmbedFields =
          [ EmbedField
              { embedFieldName = "Description",
                embedFieldValue = overview,
                embedFieldInline = Nothing
              }
          ]
            <> maybe [] (\castEntries -> [castField castEntries]) cast
        castField castEntries =
          EmbedField
            { embedFieldName = "Cast",
              embedFieldValue = castText castEntries,
              embedFieldInline = Just True
            }
        castText :: [CastEntry] -> Text
        castText castEntries =
          castEntries
            & take maxCastEntries
            & map (\CastEntry {name, character} -> mconcat ["**", name, "** as ", character])
            & Text.unlines
        createEmbedTitle =
          mconcat [title, " (", tshow voteAverage, maybe "" (", " <>) releaseDate, ")"]
        createEmbedImage =
          fmap (posterUrl imageBaseUrl posterSize >>> CreateEmbedImageUrl) posterPath
     in pure $
          Discord.def
            { createEmbedTitle,
              createEmbedFields,
              createEmbedUrl = TMDB.imdbMovieUrl imdbId,
              createEmbedImage
            }
movieEmbed _ _ _ = Nothing

movieCandidatesEmbed :: [MovieCandidate] -> CreateEmbed
movieCandidatesEmbed candidates =
  let createEmbedFields =
        candidates
          & take 10
          & fmap
            ( \MovieCandidate {title, id = MovieId id', overview, voteAverage, releaseDate} ->
                let embedFieldName =
                      mconcat
                        [maybe "" unMovieTitle title, " (", tshow id', ", ", tshow voteAverage]
                        <> maybe "" (", " <>) releaseDate
                        <> ")"
                    embedFieldValue = if overview /= "" then overview else "N/A"
                 in EmbedField {embedFieldName, embedFieldValue, embedFieldInline = Nothing}
            )
   in Discord.def {createEmbedFields}

personEmbed :: Text -> ProfileSize -> PersonCandidate -> Person -> Maybe CreateEmbed
personEmbed
  imageBaseUrl
  profileSize
  PersonCandidate {knownFor}
  Person {name = PersonName name, popularity, imdbId, profilePath, knownForDepartment} =
    let createEmbedFields = movieFields knownFor
        movieFields :: [KnownFor] -> [EmbedField]
        movieFields = fmap knownForToField
        knownForToField :: KnownFor -> EmbedField
        knownForToField (KnownForMovie KnownForMovieData {title, voteAverage, releaseDate, overview}) =
          EmbedField
            { embedFieldName =
                mconcat
                  [ maybe "" identity releaseDate,
                    ": ",
                    maybe "" identity title,
                    " (",
                    tshow voteAverage,
                    ")"
                  ],
              embedFieldValue = overview,
              embedFieldInline = Nothing
            }
        knownForToField (KnownForShow KnownForShowData {firstAirDate, overview, voteAverage, name = showName}) =
          EmbedField
            { embedFieldName =
                mconcat
                  [ maybe "" identity firstAirDate,
                    ": ",
                    maybe "" identity showName,
                    " (",
                    tshow voteAverage,
                    ")"
                  ],
              embedFieldValue = overview,
              embedFieldInline = Nothing
            }
        createEmbedImage =
          fmap (profileUrl imageBaseUrl profileSize >>> CreateEmbedImageUrl) profilePath
     in pure $
          Discord.def
            { createEmbedTitle = name,
              createEmbedFields,
              createEmbedUrl = maybe "" TMDB.imdbPersonUrl imdbId,
              createEmbedImage,
              createEmbedFooterText =
                "Known for: " <> knownForDepartment <> "    " <> "Popularity: " <> tshow popularity
            }

showEmbed :: Text -> PosterSize -> TVShow -> Maybe CreateEmbed
showEmbed
  imageBaseUrl
  posterSize
  TVShow
    { name = ShowTitle title,
      overview,
      voteAverage,
      firstAirDate,
      externalIds = ExternalIds {imdbId = Just imdbId},
      posterPath,
      credits = Credits {cast},
      nextEpisodeToAir = maybeNextEpisode,
      lastEpisodeToAir = maybeLastEpisode
    } =
    let createEmbedFields =
          [ EmbedField
              { embedFieldName = "Description",
                embedFieldValue = overview,
                embedFieldInline = Nothing
              }
          ]
            <> maybe [] (\castEntries -> [castField castEntries]) cast
            <> maybe [] (episodeField "Next Episode" >>> pure) maybeNextEpisode
            <> maybe [] (episodeField "Last Episode" >>> pure) maybeLastEpisode
        episodeField
          embedFieldName
          Episode {name, episodeNumber, seasonNumber, airDate, overview = episodeOverview} =
            let seasonEpisodePair =
                  mconcat
                    [ "S" <> Text.justifyRight 2 '0' (tshow seasonNumber),
                      "E" <> Text.justifyRight 2 '0' (tshow episodeNumber)
                    ]
                embedFieldValue =
                  mconcat
                    [ name,
                      " (",
                      seasonEpisodePair,
                      ") aired on ",
                      airDate,
                      "\n\n",
                      episodeOverview
                    ]
             in EmbedField {embedFieldName, embedFieldValue, embedFieldInline = Nothing}
        castField castEntries =
          EmbedField
            { embedFieldName = "Cast",
              embedFieldValue = castText castEntries,
              embedFieldInline = Just True
            }
        castText :: [CastEntry] -> Text
        castText [] = "N/A"
        castText castEntries =
          castEntries
            & take maxCastEntries
            & map (\CastEntry {name, character} -> mconcat ["**", name, "** as ", character])
            & Text.unlines
        createEmbedTitle =
          mconcat [title, " (", tshow voteAverage, maybe "" (", " <>) firstAirDate, ")"]
        createEmbedImage =
          fmap (posterUrl imageBaseUrl posterSize >>> CreateEmbedImageUrl) posterPath
     in pure $
          Discord.def
            { createEmbedTitle,
              createEmbedFields,
              createEmbedUrl = TMDB.imdbMovieUrl imdbId,
              createEmbedImage
            }
showEmbed _ _ _ = Nothing

showCandidatesEmbed :: [ShowCandidate] -> CreateEmbed
showCandidatesEmbed candidates =
  let createEmbedFields =
        candidates
          & take 10
          & fmap
            ( \ShowCandidate {name, id = ShowId id', overview, voteAverage, firstAirDate} ->
                let embedFieldName =
                      mconcat
                        [maybe "" unShowTitle name, " (", tshow id', ", ", tshow voteAverage]
                        <> maybe "" (", " <>) firstAirDate
                        <> ")"
                    embedFieldValue = if overview /= "" then overview else "N/A"
                 in EmbedField {embedFieldName, embedFieldValue, embedFieldInline = Nothing}
            )
   in Discord.def {createEmbedFields}

posterUrl :: Text -> PosterSize -> Text -> Text
posterUrl imageBaseUrl posterSize posterPath =
  let toUrlFragment posterSize' = removePrefix "Poster" (show posterSize') & camelCase & fromString
   in mconcat [imageBaseUrl, toUrlFragment posterSize, posterPath]

profileUrl :: Text -> ProfileSize -> Text -> Text
profileUrl imageBaseUrl profileSize profilePath =
  let toUrlFragment posterSize' = removePrefix "Profile" (show posterSize') & camelCase & fromString
   in mconcat [imageBaseUrl, toUrlFragment profileSize, profilePath]

addNewToken :: (MonadReader env m, MonadIO m, HasActiveTokens env) => User -> m UUID
addNewToken user = do
  tokensReference <- view activeTokensL
  newToken <- liftIO UUID.nextRandom
  atomically $ modifyTVar' tokensReference $ Map.insert user newToken
  pure newToken

authenticateUser ::
  (MonadReader env m, MonadIO m, HasActiveTokens env, HasAuthenticatedUsers env) =>
  User ->
  UUID ->
  m Bool
authenticateUser user token = do
  tokensReference <- view activeTokensL
  usersReference <- view authenticatedUsersL
  atomically $ do
    tokens <- readTVar tokensReference
    if Just token /= Map.lookup user tokens
      then pure False
      else do
        modifyTVar usersReference $ Set.insert user
        pure True

userIsAuthenticated :: (MonadReader env m, MonadIO m, HasAuthenticatedUsers env) => User -> m Bool
userIsAuthenticated user = do
  usersReference <- view authenticatedUsersL
  users <- readTVarIO usersReference
  pure $ user `Set.member` users

discordLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
discordLog message = logInfoS "Discord" $ display message

maxCastEntries :: Int
maxCastEntries = 20

startsWith :: Text -> Text -> Bool
startsWith = flip Text.isPrefixOf
