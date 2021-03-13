{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DiscordSandbox.Web where

import Discord.Types (User (..))
import RIO hiding (Handler)
import qualified RIO.Text as Text
import Types
import Yesod.Core (Yesod, mkYesod, Value, toJSON)
import qualified Yesod.Core as Yesod

newtype WebBase = WebBase {unWebBase :: App}

mkYesod
  "WebBase"
  [Yesod.parseRoutes|
  / RootR GET
  /users UsersR GET
|]

instance Yesod WebBase

instance HasAuthenticatedUsers WebBase where
  authenticatedUsersL =
    lens
      (\webBase -> unWebBase webBase ^. authenticatedUsersL)
      (\(WebBase app) users -> WebBase (app & authenticatedUsersL .~ users))

instance HasActiveTokens WebBase where
  activeTokensL =
    lens
      (\webBase -> unWebBase webBase ^. activeTokensL)
      (\(WebBase app) tokens -> WebBase (app & activeTokensL .~ tokens))

getRootR :: Handler Text
getRootR = do
  usersReference <- Yesod.getsYesod (^. authenticatedUsersL)
  users <- readTVarIO usersReference
  pure $ users & toList & map userName & Text.intercalate ", "

getUsersR :: Handler Value
getUsersR = do
  usersReference <- Yesod.getsYesod (^. authenticatedUsersL)
  users <- readTVarIO usersReference
  pure $ toList users & toJSON
