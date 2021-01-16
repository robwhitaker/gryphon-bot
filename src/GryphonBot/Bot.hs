module GryphonBot.Bot ( runBot, readBotTokenFromEnv ) where

import qualified System.Environment      as Env

import           Control.Exception       ( finally )

import qualified Calamity
import           Calamity                ( BotC, EventType(CustomEvt), StartupError
                                         , Tellable, Token )
import qualified Calamity.Cache.InMemory as Cache
import qualified Calamity.Commands       as Commands
import           Calamity.Commands       ( ParsePrefix )
import qualified Calamity.Metrics.Noop   as Metrics

import qualified Di
import           Di.Core                 ( Di )

import qualified DiPolysemy              as DiP

import           Optics                  ( (%), (^.) )

import qualified Polysemy                as P
import           Polysemy                ( Member, Members, Sem )
import           Polysemy.Embed          ( Embed )
import qualified Polysemy.Input          as P
import           Polysemy.Input          ( Input )
import qualified Polysemy.Reader         as P

import           Types                   ( BotConfig, BotEventChan, BotEventChanRef )

import           Habitica.Types          ( GroupChatReceivedMessage )

readBotTokenFromEnv :: String -> IO (Maybe Token)
readBotTokenFromEnv envVar = do
    fmap (Calamity.BotToken . fromString) <$> Env.lookupEnv envVar

writeInputChannel
    :: Members '[Input BotEventChanRef, Embed IO] r => Maybe BotEventChan -> Sem r ()
writeInputChannel newMbChan = do
    chanRef <- P.input
    writeIORef chanRef newMbChan

tell :: (BotC r, Tellable t) => t -> Text -> Sem r ()
tell tellable text = do
    res <- Calamity.tell tellable text
    case res of
        Right _  -> pass
        Left err -> do
            DiP.error_ $ "Could not send message to Discord: " <> show err
            pass

bot :: ( BotC r
       , Members '[ParsePrefix, Embed IO, Input BotEventChanRef, Input BotConfig] r
       )
    => Sem r ()
bot = do
    -- Share the bot's input channel so the web server can send events to the bot
    chan <- P.asks Calamity.eventsIn
    writeInputChannel (Just chan)

    -- Register bot commands
    Commands.addCommands $ do
        Commands.helpCommand

    -- Handle events from the web server
    Calamity.react
        @('CustomEvt "system-message" GroupChatReceivedMessage)
        handleSystemMessage

    pass
  where
    handleSystemMessage
        :: (BotC r, Member (Input BotConfig) r) => GroupChatReceivedMessage -> Sem r ()
    handleSystemMessage msg = do
        channelId <- P.inputs (^. #systemMessagesChannelId)
        tell channelId (msg ^. #chat % #text)

runBot :: BotConfig
       -> Token
       -> Di Di.Level Di.Path Di.Message
       -> BotEventChanRef
       -> IO (Maybe StartupError)
runBot config botToken di chanVar =
    -- Run the bot
    (P.runFinal
     . P.embedToFinal
     . DiP.runDiToIO di
     . P.runInputConst config
     . P.runInputConst chanVar
     . Cache.runCacheInMemory
     . Metrics.runMetricsNoop
     . Commands.useConstantPrefix "!"
     $ Calamity.runBotIO botToken Calamity.defaultIntents bot)
    -- The bot has exited, so remove the shared bot event channel reference
    `finally` writeIORef chanVar Nothing
