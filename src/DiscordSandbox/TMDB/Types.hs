{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DiscordSandbox.TMDB.Types where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Import

data APIQuery
  = SearchMovieQuery MovieTitle
  | SearchMovieCandidatesQuery MovieTitle
  | GetMovieQuery MovieId
  | SearchPersonQuery PersonName
  | GetPersonQuery PersonId
  | GetImageConfigurationData

data ConfigurationData = ConfigurationData
  { images :: ImageConfigurationData,
    changeKeys :: [String]
  }
  deriving (Eq, Show, Generic)

instance FromJSON ConfigurationData where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON ConfigurationData where
  toJSON value = Aeson.genericToJSON recordOptions value

data CastEntry = CastEntry
  { character :: Text,
    creditId :: Text,
    id :: Int,
    name :: Text,
    order :: Int,
    profilePath :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance FromJSON CastEntry where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON CastEntry where
  toJSON value = Aeson.genericToJSON recordOptions value

data CrewEntry = CrewEntry
  { creditId :: Text,
    department :: Text,
    id :: Int,
    name :: Text,
    job :: Text,
    profilePath :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance FromJSON CrewEntry where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON CrewEntry where
  toJSON value = Aeson.genericToJSON recordOptions value

data Credits = Credits
  { id :: Maybe Int,
    cast :: Maybe [CastEntry],
    crew :: Maybe [CrewEntry]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Credits where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON Credits where
  toJSON value = Aeson.genericToJSON recordOptions value

data Movie = Movie
  { posterPath :: Maybe Text,
    id :: MovieId,
    imdbId :: Text,
    title :: Maybe MovieTitle,
    voteAverage :: Float,
    releaseDate :: Maybe Text,
    overview :: Text,
    credits :: Credits
  }
  deriving (Eq, Show, Generic)

instance FromJSON Movie where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON Movie where
  toJSON value = Aeson.genericToJSON recordOptions value

data Episode = Episode
  { airDate :: Text,
    id :: Int,
    name :: Text,
    overview :: Text,
    seasonNumber :: Int,
    episodeNumber :: Int,
    stillPath :: Maybe String,
    voteAverage :: Float,
    voteCount :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON Episode where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON Episode where
  toJSON value = Aeson.genericToJSON recordOptions value

--struct ExternalIds {
--    imdb_id: ?String
--    freebase_mid: ?String
--    freebase_id: ?String
--    tvdb_id: ?U32
--    tvrage_id: ?U32
--}

--struct Season {
--    air_date: ?String
--    episode_count: U32
--    season_number: U32
--    id: U64
--    name: String
--    overview: String
--}
--
--struct Show {
--    poster_path: ?String
--    id: U32
--    external_ids: ExternalIds
--    name: String
--    vote_average: F32
--    first_air_date: ?String
--    overview: String
--    credits: Credits
--    next_episode_to_air: ?Episode
--    last_episode_to_air: ?Episode
--    seasons: []Season
--}

data Person = Person
  { popularity :: Float,
    name :: PersonName,
    id :: PersonId,
    profilePath :: Maybe Text,
    knownForDepartment :: Text,
    imdbId :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Person where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON Person where
  toJSON value = Aeson.genericToJSON recordOptions value

data PersonCandidate = PersonCandidate
  { popularity :: Float,
    name :: Text,
    id :: PersonId,
    profilePath :: Maybe Text,
    knownFor :: [KnownFor],
    knownForDepartment :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON PersonCandidate where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON PersonCandidate where
  toJSON value = Aeson.genericToJSON recordOptions value

data KnownForMovieData = KnownForMovieData
  { posterPath :: Maybe Text,
    id :: MovieId,
    title :: Maybe Text,
    voteAverage :: Float,
    releaseDate :: Maybe Text,
    overview :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON KnownForMovieData where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON KnownForMovieData where
  toJSON value = Aeson.genericToJSON recordOptions value

data KnownForShowData = KnownForShowData
  { posterPath :: Maybe Text,
    id :: Int,
    voteAverage :: Float,
    overview :: Text,
    firstAirDate :: Maybe Text,
    name :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON KnownForShowData where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON KnownForShowData where
  toJSON value = Aeson.genericToJSON recordOptions value

data KnownFor
  = KnownForMovie KnownForMovieData
  | KnownForShow KnownForShowData
  deriving (Eq, Show, Generic)

instance FromJSON KnownFor where
  parseJSON value =
    Aeson.withObject
      "KnownFor"
      ( \o -> do
          mediaType :: Text <- o .: "media_type"
          case mediaType of
            "movie" -> do
              knownForMovieData <- parseJSON value
              pure $ KnownForMovie knownForMovieData
            "tv" -> do
              knownForShowData <- parseJSON value
              pure $ KnownForShow knownForShowData
            _ -> error "Need 'movie' or 'tv' tag in 'media_type'"
      )
      value

instance ToJSON KnownFor where
  toJSON (KnownForMovie knownForMovieData) =
    case (Aeson.object ["media_type" .= ("movie" :: Text)], toJSON knownForMovieData) of
      (Object tag, Object knownForMovieDataObject) ->
        Object $ HashMap.union tag knownForMovieDataObject
      _ -> error "tag or `knownForMovieData` did not decode into object"
  toJSON (KnownForShow knownForShowData) =
    case (Aeson.object ["media_type" .= ("tv" :: Text)], toJSON knownForShowData) of
      (Object tag, Object knownForShowDataObject) ->
        Object $ HashMap.union tag knownForShowDataObject
      _ -> error "tag or `knownForShowData` did not decode into object"

data PersonSearchResult = PersonSearchResult
  { page :: Int,
    totalResults :: Int,
    results :: [PersonCandidate]
  }
  deriving (Eq, Show, Generic)

instance FromJSON PersonSearchResult where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON PersonSearchResult where
  toJSON value = Aeson.genericToJSON recordOptions value

data MovieSearchResult = MovieSearchResult
  { page :: Int,
    totalResults :: Int,
    results :: [MovieCandidate]
  }
  deriving (Eq, Show, Generic)

instance FromJSON MovieSearchResult where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON MovieSearchResult where
  toJSON value = Aeson.genericToJSON recordOptions value

data MovieCandidate = MovieCandidate
  { posterPath :: Maybe String,
    id :: MovieId,
    title :: Maybe MovieTitle,
    voteAverage :: Float,
    releaseDate :: Maybe Text,
    overview :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON MovieCandidate where
  parseJSON value = Aeson.genericParseJSON recordOptions value

instance ToJSON MovieCandidate where
  toJSON value = Aeson.genericToJSON recordOptions value

--struct ShowSearchResult {
--    page: U32
--    total_results: U32
--    results: []ShowCandidate
--}
