{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Web.Server (runServer) where

import qualified Calamity
import Calamity.Types.LogEff (LogEff)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Exception (bracket_)
import qualified Di
import Di.Core (Di)
import qualified DiPolysemy as DiP
import qualified Habitica.Api as Api
import Habitica.Request
  ( HabiticaAuthHeaders,
    HabiticaRequestError,
    prettyPrintError,
    retry,
    runHabiticaApi,
    urlFromError,
  )
import Habitica.Types (SenderId (..), WebhookMessage (..))
import qualified Network.HTTP.Req as Req
import qualified Network.Wai.Handler.Warp as Warp
import Optics ((%), (^.))
import Polysemy (Member, Members, Sem)
import qualified Polysemy as P
import Polysemy.Embed (Embed)
import Polysemy.Error (Error)
import qualified Polysemy.Error as P
import Polysemy.Final (Final)
import Polysemy.Input (Input)
import qualified Polysemy.Input as P
import Servant
  ( Handler (..),
    JSON,
    PostNoContent,
    QueryParam,
    ReqBody,
    ServerError,
    ServerT,
    errBody,
    (:>),
  )
import qualified Servant
import Types (BotEventChanRef, CustomEvent (ServerMessage), ServerConfig)

type API =
  "discord"
    :> "party-message"
    :> QueryParam "secret" Text
    :> ReqBody '[JSON] WebhookMessage
    :> PostNoContent

-- Concrete effects stack for the final server
type ServerEffects =
  '[ Input BotEventChanRef,
     Input ServerConfig,
     Error ServerError,
     LogEff,
     Embed IO,
     Final IO
   ]

validateWebhookEventSource ::
  Members '[LogEff, Input ServerConfig, Error ServerError] r =>
  Maybe Text ->
  Sem r ()
validateWebhookEventSource mbSecret = DiP.push "validate-req-source" $ do
  expectedSecret <- P.inputs (^. #habiticaSecret)
  case fmap (== expectedSecret) mbSecret of
    Just True -> do
      DiP.debug_ "Request source successfully validated"
    _ -> do
      DiP.warning_ "Denying request from unknown source"
      P.throw $
        Servant.err403
          { errBody = "Who are you people???"
          }

partyMessage ::
  Members
    '[ LogEff,
       Input ServerConfig,
       Input BotEventChanRef,
       Error ServerError,
       Embed IO
     ]
    r =>
  Maybe Text ->
  WebhookMessage ->
  Sem r Servant.NoContent
partyMessage mbSecret msg = DiP.push "discord" $ DiP.push "party-message" $ do
  DiP.info_ "Received a party message, processing before sending to bot"
  validateWebhookEventSource mbSecret
  mbChan <- P.input >>= P.embed . readIORef
  case mbChan of
    Nothing ->
      DiP.warning_
        "Calamity input event channel missing. Not sending message to Discord"
    Just chan -> do
      let (chatMsg, shouldSend) = case msg of
            GroupChatReceived gcr -> (gcr, gcr ^. #chat % #uuid == System)
      if shouldSend
        then do
          DiP.debug_ "Message is system message. Sending bot event."
          P.embed $
            Unagi.writeChan
              chan
              (Calamity.customEvt (ServerMessage chatMsg))
        else DiP.debug_ "Message is a user message. Ignoring."

  pure Servant.NoContent

server :: Members ServerEffects r => ServerT API (Sem r)
server = partyMessage

runServer ::
  ServerConfig ->
  HabiticaAuthHeaders ->
  Di Di.Level Di.Path Di.Message ->
  BotEventChanRef ->
  IO ()
runServer config headers di chanVar =
  bracket_ (trySetWebhookEnabled 5 True) (trySetWebhookEnabled 5 False) $ do
    Di.runDiT di $
      Di.push "server" $
        Di.notice_ $
          "Starting HTTP server on port " <> show (config ^. #port)
    liftIO $ Warp.run (config ^. #port) app
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
        . DiP.push "server"
        . DiP.attr "port" (config ^. #port)

    trySetWebhookEnabled :: Int -> Bool -> IO ()
    trySetWebhookEnabled maxAttempts enabled =
      void
        . P.runFinal
        . P.embedToFinal
        . DiP.runDiToIO di
        . logError
        . runHabiticaApi headers
        $ DiP.push "server"
        $ DiP.push phase
        $ do
          DiP.info_ $ startingOrStopping <> " system-messages webhook"
          retry maxAttempts $ Api.setWebhookEnabled enabled (config ^. #webhookId)
      where
        (startingOrStopping, phase) =
          if enabled
            then ("Starting", "setup")
            else ("Stopping", "teardown")

        logError ::
          Member LogEff r =>
          Sem (Error HabiticaRequestError ': r) a ->
          Sem r (Either HabiticaRequestError a)
        logError sem = do
          res <- P.runError sem
          case res of
            Left err -> DiP.attr "route" (Req.renderUrl $ urlFromError err) $ do
              DiP.error_ $ prettyPrintError err
              pure res
            Right _ -> pure res

    app = Servant.serve api $ Servant.hoistServer api semToHandler server
