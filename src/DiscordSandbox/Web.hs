{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module DiscordSandbox.Web where

import qualified Yesod.Core as Yesod
import RIO hiding (Handler)
import Types
import Discord.Types (User(..))
import qualified RIO.Text as Text

newtype Minimal = Minimal App

Yesod.mkYesod "Minimal" [Yesod.parseRoutes|
  / RootR GET
|]

instance Yesod.Yesod Minimal

getRootR :: Handler Text
getRootR = do
  Minimal App {appBotState = BotState {authenticated = usersReference}} <- Yesod.getYesod
  users <- readTVarIO usersReference
  pure $ users & toList & map userName & Text.intercalate ", "

