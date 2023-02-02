{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Types (BotEventChan, BotEventChanRef, CustomEvent (..), Config, ServerConfig, BotConfig) where

import Calamity (CalamityEvent, Channel, Snowflake)
import Control.Concurrent.Chan.Unagi (InChan)
import Data.Aeson (FromJSON)
import Data.UUID (UUID)
import Habitica.Types (GroupChatReceivedMessage)
import qualified Optics

-- Reference to the bot's event channel so we can send it
-- events from the server
type BotEventChanRef = IORef (Maybe BotEventChan)

type BotEventChan = InChan CalamityEvent

data CustomEvent
  = ServerMessage GroupChatReceivedMessage
  deriving stock (Typeable)

data Config = Config
  { server :: ServerConfig,
    bot :: BotConfig,
    appName :: Text,
    habiticaBotUserId :: UUID
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data ServerConfig = ServerConfig
  { habiticaSecret :: Text,
    port :: Int,
    webhookId :: UUID
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data BotConfig = BotConfig
  { systemMessagesChannelId :: Snowflake Channel
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''Config

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''ServerConfig

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''BotConfig
