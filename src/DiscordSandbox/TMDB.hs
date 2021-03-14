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
apiRequest (TMDBAPIKey key) (SearchPersonQuery (PersonName personName)) =
  let parameters =
        [ ("query", Just $ encodeUtf8 personName),
          ("language", Just "en-US"),
          ("page", Just $ fromString $ show (1 :: Int)),
          ("api_key", Just $ fromString key)
        ]
      initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest (apiBaseUrl <> "search/person")
   in initialRequest & HTTP.setQueryString parameters
apiRequest (TMDBAPIKey key) (GetPersonQuery (PersonId personId)) =
  let parameters =
        [ ("language", Just "en_US"),
          ("api_key", Just $ fromString key),
          ("append_to_response", Just "combined_credits")
        ]
      initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest (apiBaseUrl <> "person/" <> show personId)
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

searchPersonM ::
  (MonadReader env m, MonadIO m, HasTLSConnectionManager env, HasTMDBAPIKey env) =>
  PersonName ->
  m (Either String PersonCandidate)
searchPersonM personName = do
  manager <- view tlsConnectionManagerL
  apiKey <- view tmdbApiKeyL

  liftIO $ searchPerson manager apiKey personName

searchPerson :: TLSConnectionManager -> TMDBAPIKey -> PersonName -> IO (Either String PersonCandidate)
searchPerson (TLSConnectionManager manager) apiKey personName = do
  let request = apiRequest apiKey (SearchPersonQuery personName)
  body <- responseBody <$> HTTP.httpLbs request manager
  case JSON.eitherDecode' body of
    Right PersonSearchResult {results = []} -> pure $ Left "No results returned"
    Right PersonSearchResult {results = personCandidate : _rest} -> pure $ Right personCandidate
    Left error' -> pure $ Left error'

getPersonM ::
  (MonadReader env m, MonadIO m, HasTLSConnectionManager env, HasTMDBAPIKey env) =>
  PersonId ->
  m (Either String Person)
getPersonM personId = do
  manager <- view tlsConnectionManagerL
  apiKey <- view tmdbApiKeyL

  liftIO $ getPerson manager apiKey personId

getPerson :: TLSConnectionManager -> TMDBAPIKey -> PersonId -> IO (Either String Person)
getPerson (TLSConnectionManager manager) apiKey personId = do
  let request = apiRequest apiKey (GetPersonQuery personId)
  body <- responseBody <$> HTTP.httpLbs request manager

  pure $ JSON.eitherDecode' body

imdbMovieUrl :: Text -> Text
imdbMovieUrl = ("https://www.imdb.com/title/" <>)

imdbPersonUrl :: Text -> Text
imdbPersonUrl = ("https://www.imdb.com/name/" <>)

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
