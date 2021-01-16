{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Types ( BotEventChan, BotEventChanRef, Config, ServerConfig, BotConfig ) where

import           Control.Concurrent.Chan.Unagi ( InChan )

import           Data.Aeson                    ( FromJSON )
import           Data.UUID                     ( UUID )

import           Calamity                      ( CalamityEvent, Channel, Snowflake )

import qualified Optics

-- Reference to the bot's event channel so we can send it
-- events from the server
type BotEventChanRef = IORef (Maybe BotEventChan)

type BotEventChan = InChan CalamityEvent

data Config = Config
    { server            :: ServerConfig
    , bot               :: BotConfig
    , appName           :: Text
    , habiticaBotUserId :: UUID
    }
  deriving stock ( Show, Generic )
  deriving anyclass ( FromJSON )

data ServerConfig = ServerConfig
    { habiticaSecret :: Text
    , port           :: Int
    , webhookId      :: UUID
    }
  deriving stock ( Show, Generic )
  deriving anyclass ( FromJSON )

data BotConfig = BotConfig
    { systemMessagesChannelId :: Snowflake Channel
    }
  deriving stock ( Show, Generic )
  deriving anyclass ( FromJSON )

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''Config

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''ServerConfig

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''BotConfig

