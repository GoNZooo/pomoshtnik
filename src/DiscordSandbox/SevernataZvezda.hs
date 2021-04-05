{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DiscordSandbox.SevernataZvezda where

import Data.Aeson (encode, object, (.=))
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import RIO
import Types

authenticateChallengeM ::
  ( MonadReader env m,
    MonadUnliftIO m,
    HasTLSConnectionManager env,
    HasExternalAuthenticationUrl env
  ) =>
  Username ->
  AuthenticationChallenge ->
  Username ->
  m (Maybe ())
authenticateChallengeM username authenticationChallenge discordUsername = do
  connectionManager <- view tlsConnectionManagerL
  externalAuthenticationUrl <- view externalAuthenticationUrlL

  liftIO $
    authenticateChallenge
      connectionManager
      externalAuthenticationUrl
      username
      authenticationChallenge
      discordUsername

authenticateChallenge ::
  TLSConnectionManager ->
  ExternalAuthenticationUrl ->
  Username ->
  AuthenticationChallenge ->
  Username ->
  IO (Maybe ())
authenticateChallenge
  (TLSConnectionManager connectionManager)
  (ExternalAuthenticationUrl url)
  (Username username)
  (AuthenticationChallenge challenge)
  (Username discordUsername) = do
    let requestObject =
          object
            [ ("uuid" .= challenge),
              ("username" .= username),
              ("discordUsername" .= discordUsername)
            ]
        initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest ("POST " <> url)
        request =
          initialRequest
            { requestBody = RequestBodyLBS $ encode requestObject,
              requestHeaders = [("Content-Type", encodeUtf8 "application/json")]
            }
    response <- HTTP.httpLbs request connectionManager
    if HTTP.statusIsSuccessful (responseStatus response) then pure $ Just () else pure Nothing
