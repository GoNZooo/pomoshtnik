{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Discord (RunDiscordOpts (..))
import qualified Discord
import qualified Discord.Types as Discord
import Import
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified System.Environment as Environment
import Data.UUID (UUID)

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
handleMessage logFunction botState Discord.Message {Discord.messageText = messageText} = do
  newMessageCount <- liftIO $
    atomically $ do
      state@BotState {messageCount = count} <- readTVar botState
      let newCount = count + 1
      void $ swapTVar botState $ state {messageCount = newCount}
      pure newCount
  runRIO logFunction $ do
    discordLog $ "Message received (" <> textDisplay newMessageCount <> "): " <> messageText

onStart :: LogFunc -> Discord.DiscordHandler ()
onStart logFunc = do
  runRIO logFunc $ do
    discordLog "Started reading messages"

discordLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
discordLog message = logInfoS "Discord" $ display message
