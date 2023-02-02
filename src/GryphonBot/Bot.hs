{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module GryphonBot.Bot (runBot, readBotTokenFromEnv) where

import Calamity (BotC, EventType (CustomEvt), Token)
import qualified Calamity
import qualified Calamity.Cache.InMemory as Cache
import qualified Calamity.Commands as Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import qualified Calamity.Metrics.Noop as Metrics
import Calamity.Types.LogEff (LogEff)
import Control.Exception (finally)
import qualified Di
import Di.Core (Di)
import qualified DiPolysemy as DiP
import GryphonBot.Commands.LastCrons as BotCommands
import GryphonBot.Commands.QuestProgress as BotCommands
import GryphonBot.Utils (tell)
import Habitica.Request
  ( HabiticaAuthHeaders,
    HabiticaRequestError,
  )
import qualified Habitica.Request as HReq
import qualified Network.HTTP.Req as Req
import Optics ((%), (^.))
import Polysemy (Member, Members, Sem)
import qualified Polysemy as P
import Polysemy.Embed (Embed)
import Polysemy.Error
import qualified Polysemy.Error as P
import Polysemy.Input (Input)
import qualified Polysemy.Input as P
import qualified Polysemy.Reader as P
import qualified System.Environment as Env
import Types
  ( BotConfig,
    BotEventChan,
    BotEventChanRef,
    CustomEvent (ServerMessage),
  )

readBotTokenFromEnv :: String -> IO (Maybe Token)
readBotTokenFromEnv envVar = do
  fmap (Calamity.BotToken . fromString) <$> Env.lookupEnv envVar

writeInputChannel ::
  Members '[Input BotEventChanRef, Embed IO] r => Maybe BotEventChan -> Sem r ()
writeInputChannel newMbChan = do
  chanRef <- P.input
  writeIORef chanRef newMbChan

runBot ::
  BotConfig ->
  HabiticaAuthHeaders ->
  Token ->
  Di Di.Level Di.Path Di.Message ->
  BotEventChanRef ->
  IO ()
runBot config habiticaAuthHeaders botToken di chanVar =
  -- Run the bot
  ( void
      . P.runFinal
      . P.embedToFinal
      . DiP.runDiToIO di
      . P.runInputConst config
      . P.runInputConst chanVar
      . logError
      . HReq.runHabiticaApi habiticaAuthHeaders
      . Cache.runCacheInMemory
      . Metrics.runMetricsNoop
      . Commands.useConstantPrefix "!"
      . useFullContext
      . Calamity.runBotIO botToken Calamity.defaultIntents
      $ do
        chan <- P.asks Calamity.eventsIn
        writeInputChannel (Just chan)
        Commands.addCommands $ do
          Commands.helpCommand @FullContext
          Commands.commandA @'[] "questProgress" ["questprogress", "quest_progress"] (BotCommands.questProgress @FullContext)
          Commands.commandA @'[] "lastCrons" ["lastcrons", "last_crons"] (BotCommands.lastCrons @FullContext)

        -- Handle events from the web server
        void $ Calamity.react @('CustomEvt CustomEvent) handleSystemMessage
  )
    -- The bot has exited, so remove the shared bot event channel reference
    `finally` writeIORef chanVar Nothing
  where
    handleSystemMessage :: (BotC r, Member (Input BotConfig) r) => CustomEvent -> Sem r ()
    handleSystemMessage (ServerMessage msg) = do
      channelId <- P.inputs @BotConfig (^. #systemMessagesChannelId)
      tell channelId (msg ^. #chat % #text)

    -- TODO: this is duplicated from Server.hs and should be put in a
    --       shared location
    logError ::
      (Member LogEff r) =>
      Sem (Error HabiticaRequestError ': r) a ->
      Sem r (Either HabiticaRequestError a)
    logError sem = do
      res <- P.runError sem
      case res of
        Left err -> DiP.attr "route" (Req.renderUrl $ HReq.urlFromError err) $ do
          DiP.error_ $ HReq.prettyPrintError err
          pure res
        Right _ -> pure res
