{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Discord (DiscordHandler, RunDiscordOpts (..))
import qualified Discord
import Discord.Requests (ChannelRequest (..))
import Discord.Types (Event (..), Message (..))
import qualified Discord.Types as Discord
import Import
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified System.Environment as Environment

data BotState = BotState
  { authenticated :: Set Discord.User,
    activeTokens :: Map Discord.User UUID
  }

run :: RIO App ()
run = do
  token <- liftIO $ Environment.getEnv "DISCORD_API_TOKEN" >>= \t -> pure $ "Bot " <> Text.pack t
  botState <- liftIO $ newTVarIO BotState {authenticated = Set.empty, activeTokens = Map.empty}
  logFunction <- asks appLogFunc

  runDiscordResult <-
    liftIO $
      Discord.runDiscord
        Discord.def
          { discordToken = token,
            discordOnStart = onStart logFunction,
            discordOnEvent = eventHandler logFunction botState
          }
  logError $ display runDiscordResult

eventHandler :: LogFunc -> TVar BotState -> Discord.Event -> DiscordHandler ()
eventHandler logFunction botState (MessageCreate message)
  | Discord.userIsBot (Discord.messageAuthor message) = pure ()
  | otherwise = handleMessage logFunction botState message
eventHandler _ _ _ = pure ()

handleMessage :: LogFunc -> TVar BotState -> Message -> DiscordHandler ()
handleMessage logFunction botState message = do
  case decodeCommand message of
    Just command ->
      handleCommand command logFunction botState
    Nothing ->
      pure ()

data Command
  = Login Discord.ChannelId Discord.User UUID
  | GenerateToken Discord.ChannelId Discord.User
  | AuthenticatedUsers Discord.ChannelId Discord.User

decodeCommand :: Discord.Message -> Maybe Command
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

handleCommand :: Command -> LogFunc -> TVar BotState -> DiscordHandler ()
handleCommand (GenerateToken _channelId user) logFunction botState = do
  newToken <- liftIO UUID.nextRandom
  state@BotState {activeTokens = tokens} <- readTVarIO botState
  let newTokens = Map.insert user newToken tokens
  atomically $ writeTVar botState (state {activeTokens = newTokens})
  runRIO logFunction $ do
    discordLog $ "Added token '" <> tshow newToken <> "' for user with ID '" <> Discord.userName user <> "'"
  pure ()
handleCommand (Login channelId user suppliedToken) _ botState = do
  state@BotState {activeTokens = tokens, authenticated = authenticatedUsers} <- readTVarIO botState
  let userToken = Map.lookup user tokens
  case userToken of
    Just token
      | token == suppliedToken -> do
        let newState = state {authenticated = Set.insert user authenticatedUsers}
        void $ atomically $ swapTVar botState newState
        replyTo channelId user (Just "You have been authenticated.") Nothing
      | otherwise ->
        pure ()
    Nothing -> pure ()
handleCommand (AuthenticatedUsers channelId user) _ botState = do
  BotState {authenticated = authenticatedUsers} <- readTVarIO botState
  withAuthenticatedUser authenticatedUsers user $ do
    let usersString =
          Text.intercalate
            "\n"
            (Set.elems $ Set.map (\u -> "- " <> Discord.userName u <> "#" <> Discord.userDiscrim u) authenticatedUsers)
        messageEmbed =
          Discord.def
            { Discord.createEmbedFields =
                [ Discord.EmbedField
                    { Discord.embedFieldName = "Authenticated Users",
                      Discord.embedFieldValue = usersString,
                      Discord.embedFieldInline = Nothing
                    }
                ]
            }
    replyTo channelId user Nothing (Just messageEmbed)

class DiscordMention a where
  mention :: a -> Text

instance DiscordMention Discord.User where
  mention Discord.User {Discord.userId = userId} = "<@" <> tshow userId <> ">"

replyTo :: Discord.ChannelId -> Discord.User -> Maybe Text -> Maybe Discord.CreateEmbed -> DiscordHandler ()
replyTo channelId user text Nothing =
  let messageText' = mconcat [mention user, maybe "" (" " <>) text]
   in void $ Discord.restCall $ CreateMessage channelId messageText'
replyTo channelId user text (Just embed) =
  let messageText' = mconcat [mention user, maybe "" (" " <>) text]
   in void $ Discord.restCall $ CreateMessageEmbed channelId messageText' embed

withAuthenticatedUser :: Set.Set Discord.User -> Discord.User -> DiscordHandler () -> DiscordHandler ()
withAuthenticatedUser users user action
  | user `Set.member` users = action
  | otherwise = pure ()

onStart :: LogFunc -> DiscordHandler ()
onStart logFunc = do
  runRIO logFunc $ do
    discordLog "Started reading messages"

discordLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
discordLog message = logInfoS "Discord" $ display message
