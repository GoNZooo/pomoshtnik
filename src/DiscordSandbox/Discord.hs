{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiscordSandbox.Discord
  ( initialBotState,
    eventHandler,
    onStart,
    handleCommand,
  )
where

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Discord (DiscordHandle, DiscordHandler)
import qualified Discord
import Discord.Requests (ChannelRequest (..))
import Discord.Types
  ( ChannelId,
    CreateEmbed (..),
    EmbedField (..),
    Event (..),
    Message (..),
    User (..),
    createEmbedFields,
  )
import qualified Discord.Types as Discord
import Import
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text

initialBotState :: (MonadIO m) => m BotState
initialBotState = do
  authenticatedReference <- newTVarIO Set.empty
  tokensReference <- newTVarIO Map.empty
  pure $ BotState {authenticated = authenticatedReference, activeTokens = tokensReference}

decodeCommand :: Message -> Maybe Command
decodeCommand Message {messageText = text, messageAuthor = author, messageChannel = channelId}
  | text == "!generate-token" = Just $ GenerateToken channelId author
  | text == "!authenticated" = Just $ AuthenticatedUsers channelId author
  | "!login " `Text.isPrefixOf` text =
    case Text.split (== ' ') text of
      _ : token : _ ->
        case UUID.fromText token of
          Just uuid -> Just $ Login channelId author uuid
          Nothing -> Nothing
      _ -> Nothing
  | otherwise = Nothing

replyTo ::
  (MonadReader env m, HasDiscordHandle env, MonadIO m) =>
  ChannelId ->
  User ->
  Maybe Text ->
  Maybe CreateEmbed ->
  m ()
replyTo channelId user maybeText maybeEmbed = do
  runDiscordAction $ replyTo' channelId user maybeText maybeEmbed

runDiscordAction :: (MonadReader env m, MonadIO m, HasDiscordHandle env) => DiscordHandler a -> m a
runDiscordAction action = do
  discordHandleReference <- view discordHandleL
  discordHandle <- readIORef discordHandleReference
  liftIO $ runReaderT action discordHandle

addNewToken :: (MonadReader env m, HasActiveTokens env, MonadIO m) => User -> m UUID
addNewToken user = do
  tokensReference <- view activeTokensL
  newToken <- liftIO UUID.nextRandom
  atomically $ modifyTVar' tokensReference (Map.insert user newToken)
  pure newToken

authenticateUser ::
  (MonadReader env m, HasActiveTokens env, HasAuthenticatedUsers env, MonadIO m) =>
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

handleCommand ::
  ( MonadReader env m,
    MonadIO m,
    HasLogFunc env,
    HasActiveTokens env,
    HasAuthenticatedUsers env,
    HasDiscordHandle env
  ) =>
  Command ->
  m ()
handleCommand (GenerateToken _channelId user) = do
  newToken <- addNewToken user
  discordLog $ "Added token '" <> tshow newToken <> "' for user with ID '" <> userName user <> "'"
handleCommand (Login channelId user suppliedToken) = do
  authenticationSuccessful <- authenticateUser user suppliedToken
  when authenticationSuccessful $ do
    replyTo channelId user (Just "You have been authenticated.") Nothing
handleCommand (AuthenticatedUsers channelId user) = do
  usersReference <- view authenticatedUsersL
  whenM (userIsAuthenticated user) $ do
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
    replyTo channelId user Nothing (Just messageEmbed)

class DiscordMention a where
  mention :: a -> Text

instance DiscordMention User where
  mention User {userId = userId'} = "<@" <> tshow userId' <> ">"

onStart :: IORef DiscordHandle -> LogFunc -> DiscordHandler ()
onStart handleReference logFunc = do
  discordHandle <- ask
  writeIORef handleReference discordHandle
  runRIO logFunc $ do
    discordLog "Started reading messages"

discordLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
discordLog message = logInfoS "Discord" $ display message

eventHandler :: TQueue Command -> Event -> DiscordHandler ()
eventHandler commandQueue (MessageCreate message)
  | Discord.userIsBot (messageAuthor message) = pure ()
  | otherwise = handleMessage commandQueue message
eventHandler _ _ = pure ()

handleMessage :: TQueue Command -> Message -> DiscordHandler ()
handleMessage commandQueue message = do
  case decodeCommand message of
    Just command -> do
      liftIO $ atomically $ writeTQueue commandQueue command
    Nothing ->
      pure ()

replyTo' :: ChannelId -> User -> Maybe Text -> Maybe CreateEmbed -> DiscordHandler ()
replyTo' channelId user text Nothing =
  let messageText' = mconcat [mention user, maybe "" (" " <>) text]
   in void $ Discord.restCall $ CreateMessage channelId messageText'
replyTo' channelId user text (Just embed) =
  let messageText' = mconcat [mention user, maybe "" (" " <>) text]
   in void $ Discord.restCall $ CreateMessageEmbed channelId messageText' embed
