{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DiscordSandbox.Database where

import qualified Control.Monad.Logger as Logger
import Data.Pool (Pool)
import Database.Persist (Entity)
import qualified Database.Persist as Persist
import Database.Persist.Sqlite (SqlBackend, (=.))
import qualified Database.Persist.Sqlite as Sqlite
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import RIO
import Types

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Note json
      title Text
      body Text
      deriving Show
      UniqueTitle title
  |]

addNoteM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => Note -> m (Maybe (Sqlite.Key Note))
addNoteM note = do
  pool <- view sqlPoolL

  liftIO $ addNote pool note

addNote :: Pool SqlBackend -> Note -> IO (Maybe (Sqlite.Key Note))
addNote pool note = runPool pool $ Persist.insertUnique note

addToNoteM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => Sqlite.Key Note -> Text -> m (Maybe ())
addToNoteM key bodyToAdd = do
  pool <- view sqlPoolL

  liftIO $ addToNote pool key bodyToAdd

addToNote :: Pool SqlBackend -> Sqlite.Key Note -> Text -> IO (Maybe ())
addToNote pool key bodyToAdd = runPool pool $ do
  maybeNote <- Persist.get key

  case maybeNote of
    Just note -> do
      let oldBody = noteBody note
      Persist.update key [NoteBody =. oldBody <> "\n" <> bodyToAdd]
      pure $ Just ()
    Nothing -> pure Nothing

removeNoteM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => Sqlite.Key Note -> m ()
removeNoteM key = do
  pool <- view sqlPoolL

  liftIO $ removeNote pool key

removeNote :: Pool SqlBackend -> Sqlite.Key Note -> IO ()
removeNote pool key = runPool pool $ Persist.delete key

findNotesByTextM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => Text -> m [Entity Note]
findNotesByTextM text = do
  pool <- view sqlPoolL

  liftIO $ findNotesByText pool text

findNotesByText :: Pool SqlBackend -> Text -> IO [Entity Note]
findNotesByText pool text = do
  let wildcardText = Persist.PersistText $ mconcat ["%", text, "%"]

  runPool pool (Sqlite.rawSql "SELECT ?? FROM 'note' WHERE title LIKE ? OR body LIKE ?" [wildcardText, wildcardText])

runPool :: (MonadUnliftIO m) => Pool SqlBackend -> ReaderT SqlBackend m a -> m a
runPool = flip Sqlite.runSqlPool

replPool :: IO (Pool SqlBackend)
replPool = Logger.runNoLoggingT $ Sqlite.createSqlitePool (fromString "pomoshtnik.db") 8
