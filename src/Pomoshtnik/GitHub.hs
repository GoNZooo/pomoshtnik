module Pomoshtnik.GitHub where

import Network.HTTP.Client (Manager)
import Network.Wreq
import Qtility
import qualified RIO.Text as Text
import Types hiding (Options)

class (Monad m) => GitHub m where
  getGitHubUser :: Username -> m GitHubUser
  getGitHubUserRepositories :: Username -> m [GitHubRepository]
  getGitHubRepository :: Username -> RepositoryName -> m GitHubRepository

instance GitHub (RIO App) where
  getGitHubUser username = do
    manager' <- view tlsConnectionManagerL
    getGitHubUserIO manager' username
  getGitHubUserRepositories username = do
    manager' <- view tlsConnectionManagerL
    getGitHubUserRepositoriesIO manager' username
  getGitHubRepository username repositoryName = do
    manager' <- view tlsConnectionManagerL
    getGitHubRepositoryIO manager' username repositoryName

getGitHubUserIO :: (MonadIO m, MonadThrow m) => TLSConnectionManager -> Username -> m GitHubUser
getGitHubUserIO (TLSConnectionManager manager') username = do
  let command = GitHubGetUser username
  response <- liftIO $ getWith (standardOptions manager') $ urlForCommand command
  (response ^. responseBody)
    & eitherDecode
    & mapLeft (GitHubDecodingError command)
    & fromEither

getGitHubUserRepositoriesIO ::
  (MonadIO m, MonadThrow m) =>
  TLSConnectionManager ->
  Username ->
  m [GitHubRepository]
getGitHubUserRepositoriesIO (TLSConnectionManager manager') username = do
  let command = GitHubGetUserRepositories username
      getPage page = liftIO $ getWith (optionsFor page) (urlForCommand command)
      optionsFor page =
        standardOptions manager'
          & param "per_page" .~ ["100"]
          & param "sort" .~ ["updated"]
          & param "direction" .~ ["desc"]
          & param "page" .~ [Text.pack $ show @Int page]
      getAllPages currentPage = do
        response <- getPage currentPage
        case (response ^. responseBody) & eitherDecode of
          Left err -> throwM $ GitHubDecodingError command err
          Right repositories
            | length repositories < 100 -> pure repositories
            | otherwise -> do
              futureResults <- getAllPages $ currentPage + 1
              pure $ repositories <> futureResults

  getAllPages 1

getGitHubRepositoryIO ::
  (MonadIO m, MonadThrow m) =>
  TLSConnectionManager ->
  Username ->
  RepositoryName ->
  m GitHubRepository
getGitHubRepositoryIO (TLSConnectionManager manager') username repositoryName = do
  let command = GitHubGetRepository username repositoryName
  response <- liftIO $ getWith (standardOptions manager') $ urlForCommand command
  (response ^. responseBody)
    & eitherDecode
    & mapLeft (GitHubDecodingError command)
    & fromEither

apiBaseUrl :: String
apiBaseUrl = "https://api.github.com"

userUrl :: String -> String
userUrl user = apiBaseUrl <> "/users/" <> user

repositoryUrl :: String -> String
repositoryUrl repository = apiBaseUrl <> "/repos/" <> repository

urlForCommand :: GitHubCommandType -> String
urlForCommand (GitHubGetUser (Username user)) = userUrl $ Text.unpack user
urlForCommand (GitHubGetUserRepositories (Username user)) = userUrl (Text.unpack user) <> "/repos"
urlForCommand (GitHubGetRepository (Username username) (RepositoryName repository)) =
  repositoryUrl $ Text.unpack $ mconcat [username, "/", repository]

standardOptions :: Manager -> Options
standardOptions manager' =
  defaults
    & header "Accept" .~ ["application/vnd.github.v3+json"]
    & header "User-Agent" .~ ["pomoshtnik"]
    & manager .~ Right manager'
