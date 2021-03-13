{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DiscordSandbox.TMDB where

import qualified Data.Aeson as JSON
import DiscordSandbox.TMDB.Types
import Network.HTTP.Client as HTTP
import RIO
import Types

apiBaseUrl :: String
apiBaseUrl = "https://api.themoviedb.org/3/"

apiRequest :: TMDBAPIKey -> APIQuery -> Request
apiRequest (TMDBAPIKey key) (SearchMovieQuery (MovieTitle movieTitle)) =
  let parameters =
        [ ("query", Just $ encodeUtf8 movieTitle),
          ("language", Just "en-US"),
          ("page", Just $ fromString $ show (1 :: Int)),
          ("api_key", Just $ fromString key)
        ]
      initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest (apiBaseUrl <> "search/movie")
   in initialRequest & HTTP.setQueryString parameters
apiRequest (TMDBAPIKey key) (GetMovieQuery (MovieId movieId)) =
  let parameters =
        [("language", Just "en_US"), ("api_key", Just $ fromString key), ("append_to_response", Just "credits")]
      initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest (apiBaseUrl <> "movie/" <> show movieId)
   in initialRequest & HTTP.setQueryString parameters
apiRequest (TMDBAPIKey key) GetImageConfigurationData =
  let parameters = [("api_key", Just $ fromString key)]
      initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest (apiBaseUrl <> "configuration")
   in initialRequest & HTTP.setQueryString parameters

searchMovieM ::
  (MonadReader env m, MonadIO m, HasTLSConnectionManager env, HasTMDBAPIKey env) =>
  MovieTitle ->
  m (Either String Movie)
searchMovieM movieTitle = do
  manager <- view tlsConnectionManagerL
  apiKey <- view tmdbApiKeyL

  liftIO $ searchMovie manager apiKey movieTitle

searchMovie :: TLSConnectionManager -> TMDBAPIKey -> MovieTitle -> IO (Either String Movie)
searchMovie (TLSConnectionManager manager) apiKey movieTitle = do
  let request = apiRequest apiKey (SearchMovieQuery movieTitle)
  body <- responseBody <$> HTTP.httpLbs request manager
  case JSON.eitherDecode' body of
    Right MovieSearchResult {results = []} -> pure $ Left "No results returned"
    Right MovieSearchResult {results = MovieCandidate {id = movieId} : _rest} ->
      getMovie (TLSConnectionManager manager) apiKey movieId
    Left error' -> pure $ Left error'

getMovieM ::
  (MonadReader env m, MonadIO m, HasTLSConnectionManager env, HasTMDBAPIKey env) =>
  MovieId ->
  m (Either String Movie)
getMovieM movieId = do
  manager <- view tlsConnectionManagerL
  apiKey <- view tmdbApiKeyL

  liftIO $ getMovie manager apiKey movieId

getMovie :: TLSConnectionManager -> TMDBAPIKey -> MovieId -> IO (Either String Movie)
getMovie (TLSConnectionManager manager) apiKey movieId = do
  let request = apiRequest apiKey (GetMovieQuery movieId)
  body <- responseBody <$> HTTP.httpLbs request manager

  pure $ JSON.eitherDecode' body

imdbMovieUrl :: Text -> Text
imdbMovieUrl = ("https://www.imdb.com/title/" <>)

tmdbPosterUrl :: (MonadReader env m, HasTMDBImageConfigurationData env) => Text -> m Text
tmdbPosterUrl posterPath' = do
  ImageConfigurationData {secureBaseUrl = baseImageUrl} <- view tmdbImageConfigurationDataL
  pure $ baseImageUrl <> posterPath'

getImageConfigurationData :: TLSConnectionManager -> TMDBAPIKey -> IO (Either String ConfigurationData)
getImageConfigurationData (TLSConnectionManager manager) apiKey = do
  let request = apiRequest apiKey GetImageConfigurationData
  body <- liftIO $ responseBody <$> HTTP.httpLbs request manager
  case JSON.eitherDecode' body of
    Right imageConfigurationData -> pure $ pure imageConfigurationData
    Left error' -> pure $ Left error'
