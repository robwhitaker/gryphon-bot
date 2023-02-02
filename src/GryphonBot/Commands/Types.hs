{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module GryphonBot.Commands.Types (BotCommandC) where

import Calamity (BotC)
import Polysemy (Members)
import Polysemy.Fail (Fail)

type BotCommandC effs r = (BotC r, Members (Fail ': effs) r)
