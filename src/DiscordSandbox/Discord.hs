{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiscordSandbox.Discord
  ( onEvent,
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
    User (..),
  )
import Import

-- | When supplied as the `onStart` for `Discord.runDiscord` will pull out the active discord handle and fill the ref
-- with it. This allows us to use the handle in another thread outside of the normal Discord context when we want to
-- send events. `onStartAction` will be run at the end of `onStart` and can be used for logging that we started, for
-- example.
onStart :: IORef DiscordHandle -> IO () -> DiscordHandler ()
onStart handleReference onStartAction = do
  discordHandle <- ask
  writeIORef handleReference discordHandle
  liftIO onStartAction

-- | Forwards all received events to a queue of events, ideally handled in a separate thread and possibly other context.
onEvent :: TQueue Event -> Event -> DiscordHandler ()
onEvent eventQueue event = liftIO $ atomically $ writeTQueue eventQueue event

-- | Automatically embeds a user's tag at the start of a message. Supports sending text, an embed or both.
-- Requires a Discord handle in the executing context because we are using it to make REST calls.
replyTo ::
  (MonadReader env m, HasDiscordHandle env, MonadIO m) =>
  ChannelId ->
  User ->
  Maybe Text ->
  Maybe CreateEmbed ->
  m ()
replyTo channelId' user' maybeText maybeEmbed = do
  runDiscordAction $ replyTo' channelId' user' maybeText maybeEmbed

runDiscordAction :: (MonadReader env m, MonadIO m, HasDiscordHandle env) => DiscordHandler a -> m a
runDiscordAction action = do
  discordHandleReference <- view discordHandleL
  discordHandle <- readIORef discordHandleReference
  liftIO $ runReaderT action discordHandle

class DiscordMention a where
  mention :: a -> Text

instance DiscordMention User where
  mention User {userId = userId'} = "<@" <> tshow userId' <> ">"

replyTo' :: ChannelId -> User -> Maybe Text -> Maybe CreateEmbed -> DiscordHandler ()
replyTo' channelId' user' text Nothing =
  let messageText' = mconcat [mention user', maybe "" (" " <>) text]
   in void $ Discord.restCall $ CreateMessage channelId' messageText'
replyTo' channelId' user' text (Just embed) =
  let messageText' = mconcat [mention user', maybe "" (" " <>) text]
   in void $ Discord.restCall $ CreateMessageEmbed channelId' messageText' embed
