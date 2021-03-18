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
import Database.Persist (Entity (..))
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

addToNoteM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => Text -> Text -> m (Maybe ())
addToNoteM title' bodyToAdd = do
  pool <- view sqlPoolL

  liftIO $ addToNote pool title' bodyToAdd

addToNote :: Pool SqlBackend -> Text -> Text -> IO (Maybe ())
addToNote pool title' bodyToAdd = runPool pool $ do
  maybeNote <- Persist.getBy (UniqueTitle title')

  case maybeNote of
    Just (Entity noteId note) -> do
      let oldBody = noteBody note
      Persist.update noteId [NoteBody =. oldBody <> "\n\n" <> bodyToAdd]
      pure $ Just ()
    Nothing -> pure Nothing

removeNoteByTitleM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => Text -> m ()
removeNoteByTitleM title' = do
  pool <- view sqlPoolL

  liftIO $ removeNoteByTitle pool title'

removeNoteByTitle :: Pool SqlBackend -> Text -> IO ()
removeNoteByTitle pool title = runPool pool $ Persist.deleteBy (UniqueTitle title)

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
