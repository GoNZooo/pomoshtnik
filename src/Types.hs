{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.List as List
import Data.Pool (Pool)
import Data.UUID (UUID)
import qualified Database.Persist as Persist
import Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as Sql
import Discord (DiscordHandle)
import Discord.Types (ChannelId, CreateEmbed, Event, User)
import Network.HTTP.Client (Manager)
import RIO
import qualified RIO.Char as Char
import RIO.Process
import qualified RIO.Text as Text
import qualified Text.Inflections as Inflections

newtype TLSConnectionManager = TLSConnectionManager Manager

newtype TMDBAPIKey = TMDBAPIKey {unTMDBAPIKey :: String}

newtype MovieTitle = MovieTitle {unMovieTitle :: Text} deriving (Eq, Show, FromJSON, ToJSON)

newtype MovieId = MovieId {unMovieId :: Int} deriving (Eq, Show, FromJSON, ToJSON)

newtype PersonName = PersonName {unPersonName :: Text} deriving (Eq, Show, FromJSON, ToJSON)

newtype PersonId = PersonId {unPersonId :: Int} deriving (Eq, Show, FromJSON, ToJSON)

newtype ShowTitle = ShowTitle {unShowTitle :: Text} deriving (Eq, Show, FromJSON, ToJSON)

newtype ShowId = ShowId {unShowId :: Int} deriving (Eq, Show, FromJSON, ToJSON)

newtype Username = Username Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON, Persist.PersistField, Sql.PersistFieldSql)

newtype AuthenticationChallenge = AuthenticationChallenge Text
  deriving (Eq, Show, FromJSON, ToJSON)

newtype ExternalAuthenticationUrl = ExternalAuthenticationUrl String
  deriving (Eq, Show, FromJSON, ToJSON)

newtype ExternalAuthenticationToken = ExternalAuthenticationToken Text
  deriving (Eq, Show, FromJSON, ToJSON)

data InProgressNote = InProgressNote
  { title :: !Text,
    entries :: [Text]
  }

-- | Command line arguments
newtype Options = Options
  { optionsVerbose :: Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    appDiscordEvents :: TQueue Event,
    appBotState :: !BotState,
    appDiscordHandle :: IORef DiscordHandle,
    appConnectionManager :: !TLSConnectionManager,
    appTmdbApiKey :: !TMDBAPIKey,
    appTmdbImageConfigurationData :: !ImageConfigurationData,
    appSqlPool :: Pool SqlBackend,
    appNotesInProgress :: TVar (Map Username (TVar InProgressNote)),
    appExternalAuthenticationUrl :: !ExternalAuthenticationUrl,
    appExternalAuthenticationToken :: !ExternalAuthenticationToken
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

class HasTLSConnectionManager env where
  tlsConnectionManagerL :: Lens' env TLSConnectionManager

instance HasTLSConnectionManager TLSConnectionManager where
  tlsConnectionManagerL = lens id $ \_ y -> y

instance HasTLSConnectionManager App where
  tlsConnectionManagerL = lens appConnectionManager $ \x y -> x {appConnectionManager = y}

class HasTMDBAPIKey env where
  tmdbApiKeyL :: Lens' env TMDBAPIKey

instance HasTMDBAPIKey TMDBAPIKey where
  tmdbApiKeyL = lens id $ \_ y -> y

instance HasTMDBAPIKey App where
  tmdbApiKeyL = lens appTmdbApiKey $ \x y -> x {appTmdbApiKey = y}

class HasTMDBImageConfigurationData env where
  tmdbImageConfigurationDataL :: Lens' env ImageConfigurationData

instance HasTMDBImageConfigurationData App where
  tmdbImageConfigurationDataL = lens appTmdbImageConfigurationData $ \x y -> x {appTmdbImageConfigurationData = y}

class HasDiscordEventQueue env where
  discordEventQueueL :: Lens' env (TQueue Event)

instance HasDiscordEventQueue App where
  discordEventQueueL = lens appDiscordEvents (\x y -> x {appDiscordEvents = y})

class HasBotState env where
  botStateL :: Lens' env BotState

instance HasBotState App where
  botStateL = lens appBotState (\x y -> x {appBotState = y})

class HasAuthenticatedUsers env where
  authenticatedUsersL :: Lens' env (TVar (Set User))

instance HasAuthenticatedUsers App where
  authenticatedUsersL =
    lens
      (authenticated . appBotState)
      (\app@App {appBotState = botState} y -> app {appBotState = botState {authenticated = y}})

class HasActiveTokens env where
  activeTokensL :: Lens' env (TVar (Map User UUID))

instance HasActiveTokens App where
  activeTokensL =
    lens
      (activeTokens . appBotState)
      (\app@App {appBotState = botState} y -> app {appBotState = botState {activeTokens = y}})

class HasDiscordHandle env where
  discordHandleL :: Lens' env (IORef DiscordHandle)

instance HasDiscordHandle App where
  discordHandleL = lens appDiscordHandle (\x y -> x {appDiscordHandle = y})

class HasSqlPool env where
  sqlPoolL :: Lens' env (Pool SqlBackend)

instance HasSqlPool App where
  sqlPoolL = lens appSqlPool $ \x y -> x {appSqlPool = y}

class HasNotesInProgress env where
  notesInProgressL :: Lens' env (TVar (Map Username (TVar InProgressNote)))

instance HasNotesInProgress App where
  notesInProgressL = lens appNotesInProgress $ \x y -> x {appNotesInProgress = y}

class HasExternalAuthenticationUrl env where
  externalAuthenticationUrlL :: Lens' env ExternalAuthenticationUrl

instance HasExternalAuthenticationUrl App where
  externalAuthenticationUrlL =
    lens appExternalAuthenticationUrl $ \x y -> x {appExternalAuthenticationUrl = y}

class HasExternalAuthenticationToken env where
  externalAuthenticationTokenL :: Lens' env ExternalAuthenticationToken

instance HasExternalAuthenticationToken App where
  externalAuthenticationTokenL =
    lens appExternalAuthenticationToken $ \x y -> x {appExternalAuthenticationToken = y}

data BotState = BotState
  { authenticated :: TVar (Set User),
    activeTokens :: TVar (Map User UUID)
  }

data IncomingCommand = IncomingCommand {channelId :: ChannelId, user :: User, command :: Command}

data Command
  = Login UUID
  | GenerateToken
  | AuthenticatedUsers
  | SearchMovie MovieTitle
  | SearchMovieCandidates MovieTitle
  | GetMovie MovieId
  | SearchShow ShowTitle
  | SearchShowCandidates ShowTitle
  | GetShow ShowId
  | SearchPerson PersonName
  | AddNote Text Text
  | AddToNote Text Text
  | RemoveNoteByTitle Text
  | RemoveNoteByFullTextSearch Text
  | UpdateNote Text Text
  | StartNote Username Text
  | FinishNote Username
  | FullTextSearchNote Text
  | AuthenticateExternal Username AuthenticationChallenge
  deriving (Eq, Show)

data OutgoingDiscordEvent
  = SendDiscordMessage ChannelId (Maybe Text) (Maybe CreateEmbed)
  | ReplyToUser ChannelId User (Maybe Text) (Maybe CreateEmbed)

data ImageConfigurationData = ImageConfigurationData
  { baseUrl :: Text,
    secureBaseUrl :: Text,
    posterSizes :: [PosterSize],
    profileSizes :: [ProfileSize],
    stillSizes :: [StillSize],
    backdropSizes :: [BackdropSize]
  }
  deriving (Eq, Show, Generic)

instance FromJSON ImageConfigurationData where
  parseJSON value = JSON.genericParseJSON recordOptions value

instance ToJSON ImageConfigurationData where
  toJSON value = JSON.genericToJSON recordOptions value

data PosterSize
  = PosterW92
  | PosterW154
  | PosterW185
  | PosterW342
  | PosterW500
  | PosterW720
  | PosterW780
  | PosterOriginal
  deriving (Eq, Show, Generic)

instance FromJSON PosterSize where
  parseJSON value = JSON.genericParseJSON (enumerationOptions "Poster") value

instance ToJSON PosterSize where
  toJSON value = JSON.genericToJSON (enumerationOptions "Poster") value

data ProfileSize
  = ProfileW45
  | ProfileW185
  | ProfileW300
  | ProfileH632
  | ProfileOriginal
  deriving (Eq, Show, Generic)

instance FromJSON ProfileSize where
  parseJSON value = JSON.genericParseJSON (enumerationOptions "Profile") value

instance ToJSON ProfileSize where
  toJSON value = JSON.genericToJSON (enumerationOptions "Profile") value

data StillSize
  = StillW92
  | StillW185
  | StillW300
  | StillH632
  | StillOriginal
  deriving (Eq, Show, Generic)

instance FromJSON StillSize where
  parseJSON value = JSON.genericParseJSON (enumerationOptions "Still") value

instance ToJSON StillSize where
  toJSON value = JSON.genericToJSON (enumerationOptions "Still") value

data BackdropSize
  = BackdropW300
  | BackdropW780
  | BackdropW1280
  | BackdropOriginal
  deriving (Eq, Show, Generic)

instance FromJSON BackdropSize where
  parseJSON value = JSON.genericParseJSON (enumerationOptions "Backdrop") value

instance ToJSON BackdropSize where
  toJSON value = JSON.genericToJSON (enumerationOptions "Backdrop") value

enumerationOptions :: String -> JSON.Options
enumerationOptions prefix =
  JSON.defaultOptions
    { JSON.constructorTagModifier = camelCase . removePrefix prefix
    }

recordOptions :: JSON.Options
recordOptions = JSON.defaultOptions {JSON.fieldLabelModifier = camelCaseToSnakeCase}

identity :: a -> a
identity = RIO.id

removePrefix :: String -> String -> String
removePrefix prefix string = maybe string identity $ List.stripPrefix prefix string

camelCase :: String -> String
camelCase (c : rest)
  | Char.isAsciiUpper c = Char.toLower c : rest
  | otherwise = c : rest
camelCase [] = []

camelCaseToSnakeCase :: String -> String
camelCaseToSnakeCase string =
  either (const string) Text.unpack $ Inflections.toUnderscore (Text.pack string)
