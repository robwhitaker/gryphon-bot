{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module GryphonBot.Utils (tell) where

import Calamity (BotC, Tellable)
import qualified Calamity
import qualified DiPolysemy as DiP
import Polysemy (Sem)

tell :: (BotC r, Tellable t) => t -> Text -> Sem r ()
tell tellable text = do
  res <- Calamity.tell tellable text
  case res of
    Right _ -> pass
    Left err -> DiP.error_ $ "Could not send message to Discord: " <> show err
