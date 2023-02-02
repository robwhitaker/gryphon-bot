{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module GryphonBot.Commands.Utils
  ( commandWithError,
    maxTextLenBy,
    codeblock,
    yellow,
    blue,
    header,
  )
where

import Calamity (Tellable)
import qualified Data.Text as T
import qualified DiPolysemy as DiP
import GryphonBot.Commands.Types (BotCommandC)
import GryphonBot.Utils (tell)
import Habitica.Request
  ( HabiticaApi,
    HabiticaRequestError (RateLimitError),
  )
import Polysemy (Sem)
import Polysemy.Error (Error)
import qualified Polysemy.Error as P

commandWithError ::
  (BotCommandC '[HabiticaApi, Error HabiticaRequestError] r, Tellable t) =>
  (t -> Sem r ()) ->
  t ->
  Sem r ()
commandWithError cmd ctx =
  cmd ctx `P.catch` \err -> case err of
    RateLimitError _ _ -> do
      DiP.warning_
        "Hit Habitica's rate limit while processing a command; informing Discord user"
      tell ctx $
        "Looks like I've hit Habitica's rate limit and can't make any requests"
          <> " right now. Try again in a minute or so."
    _ -> do
      DiP.warning_ "Hit an error while processing a command; informing Discord user"
      tell ctx $
        "So, totally not my fault or anything, but something went wrong while"
          <> " processing your command. If this keeps happening... blame rhitakorrr."
      P.throw err

maxTextLenBy :: (a -> Text) -> [a] -> Int
maxTextLenBy thingToText = foldl' max 0 . map (T.length . thingToText)

yellow :: Text -> Text
yellow txt = "< " <> txt <> " >"

blue :: Text -> Text
blue txt = "<" <> txt <> ">"

codeblock :: Text -> Text
codeblock txt = "```md\n" <> txt <> "\n```"

header :: Text -> Text
header txt =
  mconcat $ intersperse "\n" [txt, toText $ replicate (T.length txt) '-', ""]
