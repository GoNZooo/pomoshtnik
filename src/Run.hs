{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Control.Concurrent (forkIO)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Discord (RunDiscordOpts (..))
import qualified Discord
import Discord.Types (CreateEmbed (..), EmbedField (..), Event (..), Message (..), User (..))
import DiscordSandbox.Discord (onEvent, onStart, replyTo)
import Import
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified System.Environment as Environment

run :: RIO App ()
run = do
  token <- liftIO $ Environment.getEnv "DISCORD_API_TOKEN" >>= \t -> pure $ "Bot " <> Text.pack t
  logFunction <- asks appLogFunc
  eventQueue <- asks appDiscordEvents
  handleReference <- asks appDiscordHandle
  appState <- ask

  _ <- liftIO $
    forkIO $
      forever $ do
        event <- atomically $ decodeCommand <$> readTQueue eventQueue
        runRIO appState $ maybe mempty handleCommand event

  runDiscordResult <-
    liftIO $
      Discord.runDiscord
        Discord.def
          { discordToken = token,
            discordOnStart = onStart handleReference (runRIO logFunction $ discordLog "Started reading messages"),
            discordOnEvent = onEvent eventQueue
          }
  logError $ display runDiscordResult

decodeCommand :: Event -> Maybe Command
decodeCommand (MessageCreate Message {messageText = text, messageAuthor = author, messageChannel = channelId})
  | text == "!generate-token" = Just $ GenerateToken channelId author
  | text == "!authenticated" = Just $ AuthenticatedUsers channelId author
  | "!login " `Text.isPrefixOf` text =
    case Text.split (== ' ') text of
      _ : token : _ ->
        case UUID.fromText token of
          Just uuid -> Just $ Login channelId author uuid
          Nothing -> Nothing
      _ -> Nothing
  | otherwise = Nothing
decodeCommand _ = Nothing

handleCommand ::
  ( MonadReader env m,
    MonadIO m,
    HasLogFunc env,
    HasActiveTokens env,
    HasAuthenticatedUsers env,
    HasDiscordHandle env
  ) =>
  Command ->
  m ()
handleCommand (GenerateToken _channelId user) = do
  newToken <- addNewToken user
  discordLog $ "Added token '" <> tshow newToken <> "' for user with ID '" <> userName user <> "'"
handleCommand (Login channelId user suppliedToken) = do
  whenM (authenticateUser user suppliedToken) $ do
    replyTo channelId user (Just "You have been authenticated.") Nothing
handleCommand (AuthenticatedUsers channelId user) = do
  usersReference <- view authenticatedUsersL
  whenM (userIsAuthenticated user) $ do
    authenticatedUsers <- readTVarIO usersReference
    let usersString =
          Text.intercalate
            "\n"
            (Set.elems $ Set.map (\u -> "- " <> userName u <> "#" <> userDiscrim u) authenticatedUsers)
        messageEmbed =
          Discord.def
            { createEmbedFields =
                [ EmbedField
                    { embedFieldName = "Authenticated Users",
                      embedFieldValue = usersString,
                      embedFieldInline = Nothing
                    }
                ]
            }
    replyTo channelId user Nothing (Just messageEmbed)

addNewToken :: (MonadReader env m, MonadIO m, HasActiveTokens env) => User -> m UUID
addNewToken user = do
  tokensReference <- view activeTokensL
  newToken <- liftIO UUID.nextRandom
  atomically $ modifyTVar' tokensReference (Map.insert user newToken)
  pure newToken

authenticateUser ::
  (MonadReader env m, MonadIO m, HasActiveTokens env, HasAuthenticatedUsers env) =>
  User ->
  UUID ->
  m Bool
authenticateUser user token = do
  tokensReference <- view activeTokensL
  usersReference <- view authenticatedUsersL
  atomically $ do
    tokens <- readTVar tokensReference
    if Just token /= Map.lookup user tokens
      then pure False
      else do
        modifyTVar usersReference $ Set.insert user
        pure True

userIsAuthenticated :: (MonadReader env m, MonadIO m, HasAuthenticatedUsers env) => User -> m Bool
userIsAuthenticated user = do
  usersReference <- view authenticatedUsersL
  users <- readTVarIO usersReference
  pure $ user `Set.member` users

discordLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
discordLog message = logInfoS "Discord" $ display message
