{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Di
import GryphonBot.Bot
import Habitica.Request
import Optics ((^.))
import qualified System.Environment as Env
import Types
import Web.Server

main :: IO ()
main = do
  Di.new $ \di -> do
    (config, secret, discordToken, habiticaAuthHeaders) <-
      Di.runDiT di $ Di.push "init" $ do
        !mbConfigFile <- liftIO $ Env.lookupEnv "BOT_CONFIG"
        !configFile <- case mbConfigFile of
          Just file -> pure file
          _ -> failWithLog "Could not find BOT_CONFIG in environment"

        Di.info_ $ "Parsing configuration file: " <> fromString configFile
        !(config :: Config) <- do
          eConf <- liftIO $ Aeson.eitherDecodeFileStrict' configFile
          case eConf of
            Left err ->
              failWithLog
                ("Error parsing config file: " <> fromString err)
            Right conf -> pure conf

        Di.info_ "Reading endpoint secret from environment"
        !secret <- do
          mbSecret <- liftIO $ Env.lookupEnv "SECRET"
          case mbSecret of
            Nothing -> failWithLog "Could not find SECRET in environment"
            Just s -> pure $ Secret $ toText s

        Di.info_ "Reading Discord API token from environment"
        !discordToken <- do
          mbToken <- liftIO $ readBotTokenFromEnv "DISCORD_TOKEN"
          case mbToken of
            Nothing ->
              failWithLog "Could not find DISCORD_TOKEN in environment"
            Just token -> pure token

        Di.info_ "Reading Habitica API key from environment"
        !habiticaApiKey <- do
          eKey <- liftIO $ readApiKeyFromEnv "HABITICA_API_KEY"
          case eKey of
            Left err -> failWithLog (show err)
            Right key -> pure key

        let botXClient =
              xClient (config ^. #habiticaBotUserId) (config ^. #appName)
            !habiticaAuthHeaders =
              habiticaHeaders
                (config ^. #habiticaBotUserId)
                habiticaApiKey
                botXClient

        pure (config, secret, discordToken, habiticaAuthHeaders)

    botInputChannelRef <- newIORef Nothing

    let serverAction =
          runServer (config ^. #server) secret habiticaAuthHeaders di botInputChannelRef
        botAction =
          runBot
            (config ^. #bot)
            habiticaAuthHeaders
            discordToken
            di
            botInputChannelRef

    void $ Async.withAsync botAction $ \bot -> do
      Async.withAsync serverAction $ \server -> do
        -- If one thread fails, properly cancel the other so it can call
        -- its error handlers and close cleanly
        Async.waitEitherCancel server bot
  where
    failWithLog ::
      forall a m path.
      (MonadIO m, Di.MonadDi Di.Level path Di.Message m) =>
      Di.Message ->
      m a
    failWithLog msg = do
      Di.error_ msg
      exitFailure
