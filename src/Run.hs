{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Discord (RunDiscordOpts (..))
import qualified Discord
import qualified Discord.Types as Discord
import qualified Discord.Requests as DiscordRequests
import Import
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified System.Environment as Environment

data BotState = BotState
  { messageCount :: Int,
    authenticated :: Set Discord.User,
    activeTokens :: Map Discord.User UUID
  }

run :: RIO App ()
run = do
  token <- liftIO $ Environment.getEnv "DISCORD_API_TOKEN" >>= \t -> pure $ "Bot " <> Text.pack t
  botState <- liftIO $ newTVarIO BotState {messageCount = 0, authenticated = Set.empty, activeTokens = Map.empty}
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

eventHandler :: LogFunc -> TVar BotState -> Discord.Event -> Discord.DiscordHandler ()
eventHandler logFunction botState (Discord.MessageCreate message)
  | Discord.userIsBot (Discord.messageAuthor message) = pure ()
  | otherwise = handleMessage logFunction botState message
eventHandler _ _ _ = pure ()

handleMessage :: LogFunc -> TVar BotState -> Discord.Message -> Discord.DiscordHandler ()
handleMessage logFunction botState message = do
  let messageText = Discord.messageText message
  case decodeCommand (Discord.messageChannel message) (Discord.messageAuthor message) messageText of
    Just command ->
      handleCommand command logFunction botState
    Nothing ->
      pure ()
  newMessageCount <- liftIO $ do
    atomically $ do
      state@BotState {messageCount = count} <- readTVar botState
      let newCount = count + 1
      void $ swapTVar botState $ state {messageCount = newCount}
      pure newCount
  runRIO logFunction $ do
    discordLog $ "Message received (" <> textDisplay newMessageCount <> "): " <> messageText

data Command
  = Login Discord.ChannelId Discord.User UUID
  | GenerateToken Discord.ChannelId Discord.User
  | AuthenticatedUsers Discord.ChannelId Discord.User

decodeCommand :: Discord.ChannelId -> Discord.User -> Text -> Maybe Command
decodeCommand channelId user text
  | text == "!generate-token" = Just $ GenerateToken channelId user
  | text == "!authenticated" = Just $ AuthenticatedUsers channelId user
  | "!login " `Text.isPrefixOf` text =
    case Text.split (== ' ') text of
      _ : token : _ ->
        case UUID.fromText token of
          Just uuid -> Just $ Login channelId user uuid
          Nothing -> Nothing
      _ -> Nothing
  | otherwise = Nothing

handleCommand :: Command -> LogFunc -> TVar BotState -> Discord.DiscordHandler ()
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
        void $ Discord.restCall $ DiscordRequests.CreateMessage channelId "You have been authenticated."
      | otherwise ->
        pure ()
    Nothing -> pure ()
handleCommand (AuthenticatedUsers channelId user) _ botState = do
  BotState{authenticated = authenticatedUsers} <- readTVarIO botState
  if user `Set.notMember` authenticatedUsers then pure () else do
    let usersString = Text.intercalate "\n" (Set.elems $ Set.map Discord.userName authenticatedUsers)
    void $ Discord.restCall $ DiscordRequests.CreateMessage channelId usersString

onStart :: LogFunc -> Discord.DiscordHandler ()
onStart logFunc = do
  runRIO logFunc $ do
    discordLog "Started reading messages"

discordLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
discordLog message = logInfoS "Discord" $ display message
