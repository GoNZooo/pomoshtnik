{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Discord (RunDiscordOpts (..))
import qualified Discord
import Import
import qualified RIO.Text as Text
import qualified System.Environment as Environment
import DiscordSandbox.Discord (onStart, eventHandler, initialState)

run :: RIO App ()
run = do
  token <- liftIO $ Environment.getEnv "DISCORD_API_TOKEN" >>= \t -> pure $ "Bot " <> Text.pack t
  botState <- liftIO $ newTVarIO initialState
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