{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Control.Concurrent (forkIO)
import Discord (RunDiscordOpts (..))
import qualified Discord
import DiscordSandbox.Discord (eventHandler, handleCommand, onStart)
import Import
import qualified RIO.Text as Text
import qualified System.Environment as Environment

run :: RIO App ()
run = do
  token <- liftIO $ Environment.getEnv "DISCORD_API_TOKEN" >>= \t -> pure $ "Bot " <> Text.pack t
  logFunction <- asks appLogFunc
  commandQueue <- asks appCommands
  outgoingDiscordEvents <- asks appOutgoingDiscordEvents
  appState <- ask

  _ <- liftIO $
    forkIO $
      forever $ do
        command <- atomically $ readTQueue commandQueue
        runRIO appState $ handleCommand command

  runDiscordResult <-
    liftIO $
      Discord.runDiscord
        Discord.def
          { discordToken = token,
            discordOnStart = onStart logFunction outgoingDiscordEvents,
            discordOnEvent = eventHandler commandQueue
          }
  logError $ display runDiscordResult
