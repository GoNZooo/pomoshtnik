{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified DiscordSandbox.Discord as Discord
import Import
import Options.Applicative.Simple
import qualified Paths_discord_sandbox
import RIO.Process
import Run

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_discord_sandbox.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  commands <- newTQueueIO
  botState <- Discord.initialBotState
  -- This will be filled in later in `onStart`, 
  discordHandle <- newIORef $ error "`discordHandle` reference hasn't been filled in"

  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options,
              appCommands = commands,
              appBotState = botState,
              appDiscordHandle = discordHandle
            }
     in runRIO app run
