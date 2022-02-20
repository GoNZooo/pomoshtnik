{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pomoshtnik.TMDB.Types where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Import

data APIQuery
  = SearchMovieQuery MovieTitle
  | SearchMovieCandidatesQuery MovieTitle
  | GetMovieQuery MovieId
  | SearchShowQuery ShowTitle
  | SearchShowCandidatesQuery ShowTitle
  | GetShowQuery ShowId
  | SearchPersonQuery PersonName
  | GetPersonQuery PersonId
  | GetImageConfigurationDataQuery

data ConfigurationData = ConfigurationData
  { images :: ImageConfigurationData,
    changeKeys :: [String]
  }
  deriving (Eq, Show, Generic)

instance FromJSON ConfigurationData where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON ConfigurationData where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON CastEntry where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON CrewEntry where
  toJSON = Aeson.genericToJSON recordOptions

data Credits = Credits
  { id :: Maybe Int,
    cast :: Maybe [CastEntry],
    crew :: Maybe [CrewEntry]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Credits where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON Credits where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON Movie where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON Episode where
  toJSON = Aeson.genericToJSON recordOptions

data ExternalIds = ExternalIds
  { imdbId :: Maybe Text,
    freebaseMid :: Maybe Text,
    freebaseId :: Maybe Text,
    tvdbId :: Maybe Int,
    tvRageId :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON ExternalIds where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON ExternalIds where
  toJSON = Aeson.genericToJSON recordOptions

data Season = Season
  { airDate :: Maybe Text,
    episodeCount :: Int,
    seasonNumber :: Int,
    id :: Integer,
    name :: Text,
    overview :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Season where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON Season where
  toJSON = Aeson.genericToJSON recordOptions

data TVShow = TVShow
  { posterPath :: Maybe Text,
    id :: Int,
    externalIds :: ExternalIds,
    name :: ShowTitle,
    voteAverage :: Float,
    firstAirDate :: Maybe Text,
    overview :: Text,
    credits :: Credits,
    nextEpisodeToAir :: Maybe Episode,
    lastEpisodeToAir :: Maybe Episode,
    seasons :: [Season]
  }
  deriving (Eq, Show, Generic)

instance FromJSON TVShow where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON TVShow where
  toJSON = Aeson.genericToJSON recordOptions

data Person = Person
  { popularity :: Float,
    name :: PersonName,
    id :: PersonId,
    profilePath :: Maybe Text,
    knownForDepartment :: Text,
    imdbId :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Person where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON Person where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON PersonCandidate where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON KnownForMovieData where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON KnownForShowData where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON PersonSearchResult where
  toJSON = Aeson.genericToJSON recordOptions

data MovieSearchResult = MovieSearchResult
  { page :: Int,
    totalResults :: Int,
    results :: [MovieCandidate]
  }
  deriving (Eq, Show, Generic)

instance FromJSON MovieSearchResult where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON MovieSearchResult where
  toJSON = Aeson.genericToJSON recordOptions

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
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON MovieCandidate where
  toJSON = Aeson.genericToJSON recordOptions

data ShowCandidate = ShowCandidate
  { posterPath :: Maybe String,
    id :: ShowId,
    name :: Maybe ShowTitle,
    voteAverage :: Float,
    firstAirDate :: Maybe Text,
    overview :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ShowCandidate where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON ShowCandidate where
  toJSON = Aeson.genericToJSON recordOptions

data ShowSearchResult = ShowSearchResult
  { page :: Int,
    totalResults :: Int,
    results :: [ShowCandidate]
  }
  deriving (Eq, Show, Generic)

instance FromJSON ShowSearchResult where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON ShowSearchResult where
  toJSON = Aeson.genericToJSON recordOptions
