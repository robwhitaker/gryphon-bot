module Main ( main ) where

import           Calamity                hiding ( token )
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Metrics.Noop

import qualified Di

import qualified DiPolysemy              as DiP

import qualified Polysemy                as P
import qualified Polysemy.Reader         as P

tellt :: (BotC r, Tellable t) => t -> Text -> P.Sem r (Either RestError Message)
tellt = tell

main :: IO ()
main = do
    Di.new $ \di -> void
        . P.runFinal
        . P.embedToFinal @IO
        . DiP.runDiToIO di
        . runCacheInMemory
        . runMetricsNoop
        . useConstantPrefix "!"
        $ runBotIO (BotToken "whatever") defaultIntents
        $ do
            chan <- P.asks eventsIn
            addCommands $ do
                helpCommand
                command @'[User] "utest" $ \ctx u -> do
                    void $ tellt ctx $ "got user: " <> show u
