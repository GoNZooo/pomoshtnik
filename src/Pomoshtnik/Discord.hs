{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pomoshtnik.Discord
  ( onEvent,
    onStart,
    replyTo,
    oneArgument,
    twoArguments,
    oneArgumentAndText,
    argumentsAsText,
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
import Import
import qualified RIO.List as List
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text

-- | When supplied as the `onStart` for `Discord.runDiscord` will pull out the active discord handle
-- and fill the ref with it. This allows us to use the handle in another thread outside of the
-- normal Discord context when we want to send events. `onStartAction` will be run at the end of
-- `onStart` and can be used for logging that we started, for example.
onStart :: IORef DiscordHandle -> IO () -> DiscordHandler ()
onStart handleReference onStartAction = do
  discordHandle <- ask
  writeIORef handleReference discordHandle
  liftIO onStartAction

-- | Forwards all received events to a queue of events, ideally handled in a separate thread and
-- possibly other context.
onEvent :: TQueue Event -> Event -> DiscordHandler ()
onEvent eventQueue event = liftIO $ atomically $ writeTQueue eventQueue event

-- | Automatically embeds a user's tag at the start of a message. Supports sending text, an embed
-- or both. Requires a Discord handle in the executing context because we are using it to make REST
-- calls.
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

-- | Returns the first argument in a message if there are any, `Nothing` otherwise.
oneArgument :: Message -> Maybe Text
oneArgument Message {messageText} = restWords messageText >>= List.headMaybe

-- | Returns the first two arguments in a mesage if there are any, `Nothing` otherwise.
twoArguments :: Message -> Maybe (Text, Text)
twoArguments Message {messageText} = do
  restWords' <- restWords messageText
  case restWords' of
    firstArgument : secondArgument : _ -> pure (firstArgument, secondArgument)
    _otherwise -> Nothing

-- | Returns the first argument and the concatenation of the rest in a message,
-- `Nothing` if there is no first argument.
oneArgumentAndText :: Message -> Maybe (Text, Text)
oneArgumentAndText Message {messageText} = do
  restWords' <- restWords messageText
  case restWords' of
    firstArgument : rest -> pure (firstArgument, Text.unwords rest)
    _otherwise -> Nothing

-- | Returns the concatenation of every argument in a message if there are any, `Nothing` otherwise.
argumentsAsText :: Message -> Maybe Text
argumentsAsText Message {messageText} = Text.unwords <$> restWords messageText

-- | Takes a text and returns a maybe signifying whether or not the text has words in it. This can
-- be useful in order to use it in a monadic `Maybe` context.
nonEmptyWords :: Text -> Maybe (NonEmpty Text)
nonEmptyWords text = case Text.words text of
  w : rest -> pure $ w :| rest
  [] -> Nothing

-- | Returns every word except the first in a `Text` if there are any, `Nothing` otherwise.
restWords :: Text -> Maybe [Text]
restWords text = NonEmpty.tail <$> nonEmptyWords text
