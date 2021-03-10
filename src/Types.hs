{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.UUID (UUID)
import Discord (DiscordHandle)
import Discord.Types (ChannelId, CreateEmbed, User)
import RIO
import RIO.Process

-- | Command line arguments
newtype Options = Options
  { optionsVerbose :: Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    appCommands :: TQueue Command,
    appBotState :: BotState,
    appDiscordHandle :: IORef DiscordHandle
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

class HasDiscordInbox env where
  discordInboxL :: Lens' env (TQueue Command)

instance HasDiscordInbox App where
  discordInboxL = lens appCommands (\x y -> x {appCommands = y})

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

data BotState = BotState
  { authenticated :: TVar (Set User),
    activeTokens :: TVar (Map User UUID)
  }

data Command
  = Login ChannelId User UUID
  | GenerateToken ChannelId User
  | AuthenticatedUsers ChannelId User
  deriving (Eq, Show)

data OutgoingDiscordEvent
  = SendDiscordMessage ChannelId (Maybe Text) (Maybe CreateEmbed)
  | ReplyToUser ChannelId User (Maybe Text) (Maybe CreateEmbed)
