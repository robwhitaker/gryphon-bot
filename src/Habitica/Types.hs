{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Habitica.Types
  (
    SenderId(..)
  , GroupChatReceivedMessage
  , ReceivingGroup
  , ChatMessage
  , QuestProgress
  , Party
  , Quest
  , WebhookMessage(..)
  , Member
  , MemberAuth
  , MemberAuthLocal
  , MemberAuthTimestamps
  , MemberItems
  , MemberParty
  , MemberQuestStatus
  , PendingQuestProgress
  , MemberProfile ) where

import           Data.Aeson      ( (.:), (.:?), FromJSON )
import qualified Data.Aeson      as Aeson
import           Data.Time.Clock ( UTCTime )
import           Data.UUID       ( UUID )
import qualified Data.UUID       as UUID

import qualified Optics.TH       as Optics

-- Quest progress from party
newtype Party = Party
    { quest :: Quest
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

data Quest = Quest
    { progress :: !QuestProgress
    , active   :: !Bool
    , members  :: !(Map UUID Bool)
    , key      :: !Text
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

data QuestProgress = QuestProgress
    { collect :: !(Map Text Int)
    , hp      :: !Double
    , rage    :: !Double
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

-- Chat messages from group chat
data ChatMessage = ChatMessage
    { uuid :: !SenderId
    , text :: !Text
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

data SenderId
    = System
    | User !UUID
  deriving stock ( Show, Eq )

instance FromJSON SenderId where
    parseJSON = Aeson.withText "SenderId" $ \txt -> case UUID.fromText txt of
        Nothing   -> return System
        Just uuid -> return (User uuid)

-- Messages from Habitica's webhooks
newtype WebhookMessage = GroupChatReceived GroupChatReceivedMessage
  deriving stock ( Show, Eq )

instance FromJSON WebhookMessage where
    parseJSON = Aeson.withObject "WebhookMessage" $ \o -> do
        webhookType :: Text <- o .: "webhookType"
        case webhookType of
            "groupChatReceived" -> GroupChatReceived
                <$> Aeson.parseJSON (Aeson.Object o)

            _                   -> fail "Invalid webhook type"

data GroupChatReceivedMessage = GroupChatReceivedMessage
    { group :: !ReceivingGroup
    , chat  :: !ChatMessage
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

data ReceivingGroup = ReceivingGroup
    { id   :: !UUID
    , name :: !Text
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

-- Member data
data Member = Member
    { id      :: !UUID
    , auth    :: !MemberAuth
    , items   :: !MemberItems
    , party   :: !MemberParty
    , profile :: !MemberProfile
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

newtype MemberProfile = MemberProfile
    { name :: Text
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

data MemberAuth = MemberAuth
    { local      :: !MemberAuthLocal
    , timestamps :: !MemberAuthTimestamps
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

newtype MemberAuthLocal = MemberAuthLocal
    { username :: Text
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

data MemberAuthTimestamps = MemberAuthTimeStamps
    { created  :: !UTCTime
    , loggedin :: !UTCTime
    , updated  :: !UTCTime
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

newtype MemberItems = MemberItems
    { quests :: Map Text Int
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

newtype MemberParty = MemberParty
    { quest :: MemberQuestStatus
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

data MemberQuestStatus = MemberQuestStatus
    { progress   :: !PendingQuestProgress
    , key        :: !(Maybe Text)
    , rsvpNeeded :: !Bool
    }
  deriving stock ( Show, Eq )

instance FromJSON MemberQuestStatus where
    parseJSON = Aeson.withObject "MemberQuestStatus" $ \o -> do
        MemberQuestStatus <$> o .: "progress" <*> o .:? "key" <*> o .: "RSVPNeeded"

data PendingQuestProgress = PendingQuestProgress
    { up             :: !Double
    , collectedItems :: !Int
    , collect        :: !(Map Text Int)
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''Party

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''Quest

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''QuestProgress

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''ChatMessage

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''GroupChatReceivedMessage

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''ReceivingGroup

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''Member

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''MemberAuth

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''MemberAuthLocal

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''MemberAuthTimestamps

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''MemberItems

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''MemberParty

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''MemberQuestStatus

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''PendingQuestProgress

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''MemberProfile
