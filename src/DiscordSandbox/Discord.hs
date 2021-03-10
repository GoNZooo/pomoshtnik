{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiscordSandbox.Discord
  ( initialBotState,
    eventHandler,
    onStart,
    replyTo,
  )
where

import Discord (DiscordHandle, DiscordHandler)
import qualified Discord
import Discord.Requests (ChannelRequest (..))
import Discord.Types
  ( ChannelId,
    CreateEmbed (..),
    Event (..),
    Message (..),
    User (..),
  )
import qualified Discord.Types as Discord
import Import
import qualified RIO.Map as Map
import qualified RIO.Set as Set

initialBotState :: (MonadIO m) => m BotState
initialBotState = do
  authenticatedReference <- newTVarIO Set.empty
  tokensReference <- newTVarIO Map.empty
  pure $ BotState {authenticated = authenticatedReference, activeTokens = tokensReference}

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

class DiscordMention a where
  mention :: a -> Text

instance DiscordMention User where
  mention User {userId = userId'} = "<@" <> tshow userId' <> ">"

onStart :: IORef DiscordHandle -> LogFunc -> ReaderT LogFunc IO () -> DiscordHandler ()
onStart handleReference logFunc onStartAction = do
  discordHandle <- ask
  writeIORef handleReference discordHandle
  void $ liftIO $ runReaderT onStartAction logFunc

eventHandler :: (Message -> Maybe command) -> TQueue command -> Event -> DiscordHandler ()
eventHandler commandDecoder commandQueue (MessageCreate message)
  | Discord.userIsBot (messageAuthor message) = pure ()
  | otherwise = handleMessage commandDecoder commandQueue message
eventHandler _ _ _ = pure ()

handleMessage :: (Message -> Maybe command) -> TQueue command -> Message -> DiscordHandler ()
handleMessage commandDecoder commandQueue message = do
  case commandDecoder message of
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
