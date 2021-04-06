{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pomoshtnik.SevernataZvezda where

import Data.Aeson (encode, object, (.=))
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import RIO
import Types

authenticateChallengeM ::
  ( MonadReader env m,
    MonadUnliftIO m,
    HasTLSConnectionManager env,
    HasExternalAuthenticationUrl env,
    HasExternalAuthenticationToken env
  ) =>
  Username ->
  AuthenticationChallenge ->
  Username ->
  m (Maybe ())
authenticateChallengeM username authenticationChallenge discordUsername = do
  authenticationToken <- view externalAuthenticationTokenL
  connectionManager <- view tlsConnectionManagerL
  externalAuthenticationUrl <- view externalAuthenticationUrlL

  liftIO $
    authenticateChallenge
      connectionManager
      externalAuthenticationUrl
      username
      authenticationChallenge
      discordUsername
      authenticationToken

authenticateChallenge ::
  TLSConnectionManager ->
  ExternalAuthenticationUrl ->
  Username ->
  AuthenticationChallenge ->
  Username ->
  ExternalAuthenticationToken ->
  IO (Maybe ())
authenticateChallenge
  (TLSConnectionManager connectionManager)
  (ExternalAuthenticationUrl url)
  (Username username)
  (AuthenticationChallenge challenge)
  (Username discordUsername)
  (ExternalAuthenticationToken authenticationToken) = do
    let requestObject =
          object
            [ "uuid" .= challenge,
              "username" .= username,
              "discordUsername" .= discordUsername,
              "token" .= tokenObject
            ]
        tokenObject = object ["key" .= authenticationToken, "service" .= serviceName]
        initialRequest = maybe HTTP.defaultRequest RIO.id $ HTTP.parseRequest ("POST " <> url)
        request =
          initialRequest
            { requestBody = RequestBodyLBS $ encode requestObject,
              requestHeaders = [("Content-Type", encodeUtf8 "application/json")]
            }
    response <- HTTP.httpLbs request connectionManager
    if HTTP.statusIsSuccessful (responseStatus response) then pure $ Just () else pure Nothing

serviceName :: Text
serviceName = "pomoshtnik"
