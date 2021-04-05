{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified Control.Monad.Logger as Logger
import qualified Database.Persist.Sqlite as Sqlite
import qualified DiscordSandbox.Database as Database
import qualified DiscordSandbox.TMDB as TMDB
import qualified DiscordSandbox.TMDB.Types as TMDB
import Import
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative.Simple
import qualified Paths_discord_sandbox
import RIO.Process
import Run
import qualified System.Environment as Environment

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
  events <- newTQueueIO
  botState <- initialBotState
  -- This will be filled in later in `onStart`,
  discordHandle <- newIORef $ error "`discordHandle` reference hasn't been filled in"
  connectionManager <- TLSConnectionManager <$> newTlsManager
  tmdbApiKey <- TMDBAPIKey <$> Environment.getEnv "TMDB_API_KEY"
  tmdbImageConfigurationData <-
    either (\error' -> error $ "Unable to get TMDB image configuration data: " <> error') TMDB.images
      <$> TMDB.getImageConfigurationData connectionManager tmdbApiKey
  externalAuthenticationUrl <-
    ExternalAuthenticationUrl <$> Environment.getEnv "EXTERNAL_AUTHENTICATION_URL"
  externalAuthenticationToken <-
    ExternalAuthenticationToken . fromString <$> Environment.getEnv "EXTERNAL_AUTHENTICATION_TOKEN"
  pool <- Logger.runNoLoggingT $ Sqlite.createSqlitePool (fromString "pomoshtnik.db") 8
  Sqlite.runSqlPool (Sqlite.runMigration Database.migrateAll) pool
  notesInProgress <- newTVarIO mempty

  withLogFunc lo $ \lf -> do
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options,
              appDiscordEvents = events,
              appBotState = botState,
              appDiscordHandle = discordHandle,
              appConnectionManager = connectionManager,
              appTmdbApiKey = tmdbApiKey,
              appTmdbImageConfigurationData = tmdbImageConfigurationData,
              appSqlPool = pool,
              appNotesInProgress = notesInProgress,
              appExternalAuthenticationUrl = externalAuthenticationUrl,
              appExternalAuthenticationToken = externalAuthenticationToken
            }
    runRIO app run

initialBotState :: (MonadIO m) => m BotState
initialBotState = do
  authenticatedReference <- newTVarIO mempty
  tokensReference <- newTVarIO mempty
  pure $ BotState {authenticated = authenticatedReference, activeTokens = tokensReference}
