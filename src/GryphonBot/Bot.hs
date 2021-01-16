module GryphonBot.Bot ( runBot, readBotTokenFromEnv ) where

import qualified System.Environment                as Env

import           Control.Exception                 ( finally )

import qualified Calamity
import           Calamity                          ( BotC, EventType(CustomEvt), Token )
import qualified Calamity.Cache.InMemory           as Cache
import qualified Calamity.Commands                 as Commands
import           Calamity.Commands                 ( ParsePrefix )
import qualified Calamity.Metrics.Noop             as Metrics
import           Calamity.Types.LogEff             ( LogEff )

import qualified Di
import           Di.Core                           ( Di )

import qualified DiPolysemy                        as DiP

import qualified Network.HTTP.Req                  as Req

import           Optics                            ( (%), (^.) )

import qualified Polysemy                          as P
import           Polysemy                          ( Member, Members, Sem )
import           Polysemy.Embed                    ( Embed )
import qualified Polysemy.Error                    as P
import           Polysemy.Error
import qualified Polysemy.Input                    as P
import           Polysemy.Input                    ( Input )
import qualified Polysemy.Reader                   as P

import           Types                             ( BotConfig, BotEventChan
                                                   , BotEventChanRef )

import           GryphonBot.Commands.QuestProgress as BotCommands
import           GryphonBot.Utils                  ( tell )

import qualified Habitica.Request                  as HReq
import           Habitica.Request                  ( HabiticaApi, HabiticaAuthHeaders
                                                   , HabiticaRequestError )
import           Habitica.Types                    ( GroupChatReceivedMessage )

readBotTokenFromEnv :: String -> IO (Maybe Token)
readBotTokenFromEnv envVar = do
    fmap (Calamity.BotToken . fromString) <$> Env.lookupEnv envVar

writeInputChannel
    :: Members '[Input BotEventChanRef, Embed IO] r => Maybe BotEventChan -> Sem r ()
writeInputChannel newMbChan = do
    chanRef <- P.input
    writeIORef chanRef newMbChan

bot :: forall r.
    ( BotC r
    , Members
          '[ ParsePrefix
           , Embed IO
           , Input BotEventChanRef
           , Input BotConfig
           , HabiticaApi
           , Error HabiticaRequestError
           , LogEff
           ]
          r
    )
    => Sem r ()
bot = do
    -- Share the bot's input channel so the web server can send events to the bot
    chan <- P.asks Calamity.eventsIn
    writeInputChannel (Just chan)

    -- Register bot commands
    Commands.addCommands $ do
        Commands.helpCommand
        Commands.commandA
            @'[]
            "questProgress"
            ["questprogress", "quest_progress"]
            BotCommands.questProgress

    -- Handle events from the web server
    Calamity.react
        @('CustomEvt "system-message" GroupChatReceivedMessage)
        handleSystemMessage

    pass
  where
    handleSystemMessage :: GroupChatReceivedMessage -> Sem r ()
    handleSystemMessage msg = do
        channelId <- P.inputs @BotConfig (^. #systemMessagesChannelId)
        tell channelId (msg ^. #chat % #text)

runBot :: BotConfig
       -> HabiticaAuthHeaders
       -> Token
       -> Di Di.Level Di.Path Di.Message
       -> BotEventChanRef
       -> IO ()
runBot config habiticaAuthHeaders botToken di chanVar =
    -- Run the bot
    (void
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
     $ Calamity.runBotIO botToken Calamity.defaultIntents bot)
    -- The bot has exited, so remove the shared bot event channel reference
    `finally` writeIORef chanVar Nothing
  where
    -- TODO: this is duplicated from Server.hs and should be put in a
    --       shared location
    logError :: Member LogEff r
             => Sem (Error HabiticaRequestError ': r) a
             -> Sem r (Either HabiticaRequestError a)
    logError sem = do
        res <- P.runError sem
        case res of
            Left err -> DiP.attr "route" (Req.renderUrl $ HReq.urlFromError err) $ do
                DiP.error_ $ HReq.prettyPrintError err
                pure res
            Right _  -> pure res
