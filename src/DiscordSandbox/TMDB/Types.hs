{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DiscordSandbox.TMDB.Types where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Import

data APIQuery
  = SearchMovieQuery MovieTitle
  | GetMovieQuery MovieId
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
    id :: Int,
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
--
--struct Person {
--    popularity: F32
--    name: String
--    id: U32
--    profile_path: ?String
--    known_for_department: String
--    imdb_id: String
--}
--
--
--struct ShowCandidate {
--    poster_path: ?String
--    id: U32
--    name: String
--    vote_average: F32
--    first_air_date: ?String
--    overview: String
--}
--
--struct KnownForMovieData {
--    poster_path: ?String
--    id: U32
--    title: ?String
--    vote_average: F32
--    release_date: ?String
--    overview: String
--}
--
--struct KnownForShowData {
--    poster_path: ?String
--    id: U32
--    vote_average: F32
--    overview: String
--    first_air_date: ?String
--    name: ?String
--}
--
--union(tag = media_type, embedded) KnownFor {
--    movie: KnownForMovieData
--    tv: KnownForShowData
--}
--
--struct PersonCandidate {
--    popularity: F32
--    name: String
--    id: U32
--    profile_path: ?String
--    known_for: []KnownFor
--    known_for_department: String
--}
--
--struct PersonSearchResult {
--    page: U32
--    total_results: U32
--    results: []PersonCandidate
--}

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
