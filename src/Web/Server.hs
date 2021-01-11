module Web.Server ( runServer, Config(..) ) where

import           Control.Concurrent.Chan.Unagi ( InChan )
import qualified Control.Concurrent.Chan.Unagi as Unagi

import           Calamity                      ( CalamityEvent )
import qualified Calamity
import           Calamity.Types.LogEff         ( LogEff )

import qualified Di
import           Di.Core                       ( Di )

import qualified DiPolysemy                    as DiP

import qualified Network.Wai.Handler.Warp      as Warp

import           Optics                        ( (%), (^.) )

import qualified Polysemy                      as P
import           Polysemy                      ( Members, Sem )
import           Polysemy.Embed                ( Embed )
import           Polysemy.Error                ( Error )
import qualified Polysemy.Error                as P
import           Polysemy.Final                ( Final )
import qualified Polysemy.Input                as P
import           Polysemy.Input                ( Input )

import qualified Servant
import           Servant                       ( (:>), Handler(..), JSON, PostNoContent
                                               , QueryParam, ReqBody, ServerError
                                               , ServerT, errBody )

import           Habitica.Types                ( SenderId(..), WebhookMessage(..) )

-- TODO: Replace with proper config type
data Config = Config
    { habiticaSecret :: Text
    , port           :: Int
    }

type API =
    "discord" :> "party-message"
    :> QueryParam "secret" Text :> ReqBody '[JSON] WebhookMessage :> PostNoContent

-- Reference to the bot's event channel so we can send it
-- events from the server
type BotEventChanRef =
    IORef (Maybe (InChan CalamityEvent))

-- Concrete effects stack for the final server
type ServerEffects =
    '[ Input BotEventChanRef
     , Input Config
     , Error ServerError
     , LogEff
     , Embed IO
     , Final IO
     ]

validateWebhookEventSource :: Members '[LogEff, Input Config, Error ServerError] r
                           => Maybe Text
                           -> Sem r ()
validateWebhookEventSource mbSecret = DiP.push "validate-req-source" $ do
    expectedSecret <- P.inputs habiticaSecret
    case fmap (== expectedSecret) mbSecret of
        Just True -> do
            DiP.info_ "Request source successfully validated"

        _         -> do
            DiP.notice_ "Denying request from unknown source"
            P.throw
                $ Servant.err403
                { errBody = "Who are you people???" }

partyMessage :: Members
                 '[ LogEff
                  , Input Config
                  , Input BotEventChanRef
                  , Error ServerError
                  , Embed IO
                  ]
                 r
             => Maybe Text
             -> WebhookMessage
             -> Sem r Servant.NoContent
partyMessage mbSecret msg = DiP.push "party-message" $ do
    validateWebhookEventSource mbSecret
    mbChan <- P.input >>= P.embed . readIORef
    case mbChan of
        Nothing   -> DiP.warning_
            "Calamity input event channel missing. Not sending message to Discord"

        Just chan -> do
            let (chatMsg,shouldSend) = case msg of
                    GroupChatReceived gcr -> (gcr, gcr ^. #chat % #uuid == System)
            if shouldSend
                then do
                    DiP.info_ "Message is system message. Sending bot event."
                    P.embed
                        $ Unagi.writeChan
                            chan
                            (Calamity.customEvt @"system-message" chatMsg)
                else DiP.info_ "Message is a user message. Ignoring."

    pure Servant.NoContent

server :: Members ServerEffects r
       => ServerT API (Sem r)
server = partyMessage

runServer :: Config -> Di Di.Level Di.Path Di.Message -> BotEventChanRef -> IO ()
runServer config di chanVar =
    Warp.run (port config) app
  where
    api = Proxy @API

    semToHandler :: Sem ServerEffects a -> Handler a
    semToHandler =
        Handler
        . ExceptT
        . P.runFinal
        . P.embedToFinal
        . DiP.runDiToIO di
        . P.runError
        . P.runInputConst config
        . P.runInputConst chanVar

    app = Servant.serve api $ Servant.hoistServer api semToHandler server
