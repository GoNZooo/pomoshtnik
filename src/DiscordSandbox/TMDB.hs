{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DiscordSandbox.TMDB where

import qualified Data.Aeson as JSON
import DiscordSandbox.TMDB.Types
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import RIO

apiBaseUrl :: String
apiBaseUrl = "https://api.themoviedb.org/3/"

apiRequest :: APIKey -> APIQuery -> Request
apiRequest (APIKey key) (SearchMovie (MovieTitle movieTitle)) =
  let parameters =
        [("query", Just $ encodeUtf8 movieTitle), ("language", Just "en_US"), ("api_key", Just $ fromString key)]
      initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest (apiBaseUrl <> "search/movie")
   in initialRequest & HTTP.setQueryString parameters
apiRequest (APIKey key) (GetMovie (MovieId movieId)) =
  let parameters =
        [("language", Just "en_US"), ("api_key", Just $ fromString key), ("append_to_response", Just "credits")]
      initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest (apiBaseUrl <> "movie/" <> show movieId)
   in initialRequest & HTTP.setQueryString parameters

searchMovie :: APIKey -> MovieTitle -> IO (Either String Movie)
searchMovie apiKey movieTitle = do
  manager <- HTTP.newTlsManager
  let request = apiRequest apiKey (SearchMovie movieTitle)
  body <- responseBody <$> HTTP.httpLbs request manager
  case JSON.eitherDecode' body of
    Right MovieSearchResult {results = []} -> pure $ Left "No results returned"
    Right MovieSearchResult {results = MovieCandidate {id = movieId} : _rest} -> getMovie apiKey movieId
    Left error' -> pure $ Left error'

getMovie :: APIKey -> MovieId -> IO (Either String Movie)
getMovie apiKey movieId = do
  manager <- HTTP.newTlsManager
  let request = apiRequest apiKey (GetMovie movieId)
  body <- responseBody <$> HTTP.httpLbs request manager
  pure $ JSON.eitherDecode' body
