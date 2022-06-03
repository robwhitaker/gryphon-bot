module Habitica.Api
  ( -- * Query Habitica
    fetchParty,
    fetchPartyMembers,

    -- * Modify webhooks
    setWebhookEnabled,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Habitica.Request
import Habitica.Types
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (..),
    PUT (PUT),
    ReqBodyJson (..),
    (=:),
  )
import Polysemy (Sem)
import qualified Polysemy as P

fetchParty :: P.Member HabiticaApi r => Sem r (HabiticaResponse Party)
fetchParty = habiticaRequest GET ["groups", "party"] NoReqBody mempty

fetchPartyMembers :: P.Member HabiticaApi r => Sem r (HabiticaResponse [Member])
fetchPartyMembers =
  habiticaRequest
    GET
    ["groups", "party", "members"]
    NoReqBody
    ("includeAllPublicFields" =: True)

setWebhookEnabled ::
  P.Member HabiticaApi r => Bool -> UUID -> Sem r (HabiticaResponse IgnoreData)
setWebhookEnabled enable webhookId =
  habiticaRequest
    PUT
    ["user", "webhook", UUID.toText webhookId]
    (ReqBodyJson $ Aeson.object ["enabled" .= enable])
    mempty
