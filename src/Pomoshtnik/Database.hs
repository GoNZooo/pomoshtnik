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

module Pomoshtnik.Database where

import qualified Control.Monad.Logger as Logger
import Data.Pool (Pool)
import Database.Persist (Entity (..), (=.), (==.))
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Sql
import Database.Persist.Sqlite (SqlBackend)
import qualified Database.Persist.Sqlite as Sqlite
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import RIO
import Types

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  User json
      username Username
      deriving Show
      UniqueUsername username
  Note json
      title Text
      body Text
      userId UserId
      deriving Show
      UniqueUserIdTitle userId title
  |]

getOrCreateUserM :: (MonadReader env m, MonadUnliftIO m, HasSqlPool env) => Username -> m (Maybe (Entity User))
getOrCreateUserM username = do
  pool <- view sqlPoolL

  liftIO $ getOrCreateUser pool username

getOrCreateUser :: Pool SqlBackend -> Username -> IO (Maybe (Entity User))
getOrCreateUser pool username = do
  maybeUser <- maybeGetUser pool username
  case maybeUser of
    Just user' -> pure $ Just user'
    Nothing -> do
      runPool pool $ do
        maybeUserId <- Persist.insertUnique $ User {userUsername = username}
        case maybeUserId of
          Just userId -> Persist.getEntity userId
          Nothing -> pure Nothing

maybeGetUserM :: (MonadReader env m, MonadUnliftIO m, HasSqlPool env) => Username -> m (Maybe (Entity User))
maybeGetUserM username = do
  pool <- view sqlPoolL

  liftIO $ maybeGetUser pool username

maybeGetUser :: Pool SqlBackend -> Username -> IO (Maybe (Entity User))
maybeGetUser pool username = runPool pool $ do
  Persist.getBy (UniqueUsername username)

addNoteM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => Note -> m (Maybe (Persist.Key Note))
addNoteM note = do
  pool <- view sqlPoolL

  liftIO $ addNote pool note

addNote :: Pool SqlBackend -> Note -> IO (Maybe (Persist.Key Note))
addNote pool note = runPool pool $ Persist.insertUnique note

addToNoteM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => UserId -> Text -> Text -> m (Maybe ())
addToNoteM userId title' bodyToAdd = do
  pool <- view sqlPoolL

  liftIO $ addToNote pool userId title' bodyToAdd

addToNote :: Pool SqlBackend -> UserId -> Text -> Text -> IO (Maybe ())
addToNote pool userId title' bodyToAdd = runPool pool $ do
  maybeNote <- Persist.getBy (UniqueUserIdTitle userId title')

  case maybeNote of
    Just (Entity noteId note) -> do
      let oldBody = noteBody note
      Persist.update noteId [NoteBody =. oldBody <> "\n\n" <> bodyToAdd]
      pure $ Just ()
    Nothing -> pure Nothing

removeNoteByTitleM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => UserId -> Text -> m ()
removeNoteByTitleM userId title' = do
  pool <- view sqlPoolL

  liftIO $ removeNoteByTitle pool userId title'

removeNoteByTitle :: Pool SqlBackend -> UserId -> Text -> IO ()
removeNoteByTitle pool userId title' = runPool pool $ Persist.deleteBy (UniqueUserIdTitle userId title')

removeNoteByFullTextSearchM :: (MonadReader env m, MonadUnliftIO m, HasSqlPool env) => UserId -> Text -> m ()
removeNoteByFullTextSearchM userId title' = do
  pool <- view sqlPoolL

  liftIO $ removeNoteByFullTextSearch pool userId title'

removeNoteByFullTextSearch :: Pool SqlBackend -> UserId -> Text -> IO ()
removeNoteByFullTextSearch pool userId title' =
  runPool pool $ Persist.deleteWhere [NoteUserId ==. userId, fullTextNoteFilter title']

updateNoteM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => Note -> m ()
updateNoteM note = do
  pool <- view sqlPoolL

  liftIO $ updateNote pool note

updateNote :: Pool SqlBackend -> Note -> IO ()
updateNote pool note = runPool pool $ Persist.updateWhere [NoteTitle ==. noteTitle note] [NoteBody =. noteBody note]

findNotesByTextM :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env) => UserId -> Text -> m [Entity Note]
findNotesByTextM userId text = do
  pool <- view sqlPoolL

  liftIO $ findNotesByText pool userId text

findNotesByText :: Pool SqlBackend -> UserId -> Text -> IO [Entity Note]
findNotesByText pool userId text = do
  runPool pool $ Persist.selectList [NoteUserId ==. userId, fullTextNoteFilter text] []

fullTextNoteFilter :: Text -> Persist.Filter Note
fullTextNoteFilter text =
  let wildcardText = Persist.FilterValue $ mconcat ["%", text, "%"]
      titleFilter = Persist.Filter NoteTitle wildcardText $ Persist.BackendSpecificFilter "LIKE"
      bodyFilter = Persist.Filter NoteBody wildcardText $ Persist.BackendSpecificFilter "LIKE"
   in Persist.FilterOr [titleFilter, bodyFilter]

runPool :: (MonadUnliftIO m) => Pool SqlBackend -> ReaderT SqlBackend m a -> m a
runPool = flip Sql.runSqlPool

replPool :: IO (Pool SqlBackend)
replPool = Logger.runNoLoggingT $ Sqlite.createSqlitePool (fromString "pomoshtnik.db") 8
