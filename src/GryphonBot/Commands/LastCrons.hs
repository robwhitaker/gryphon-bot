module GryphonBot.Commands.LastCrons (lastCrons) where

import Calamity (Tellable)
import qualified Data.Text as T (justifyLeft)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import Di (Path)
import qualified DiPolysemy as DiP (debug_, info_)
import GryphonBot.Commands.Types (BotCommandC)
import GryphonBot.Commands.Utils
  ( codeblock,
    commandWithError,
    header,
    maxTextLenBy,
    yellow,
  )
import GryphonBot.Utils (tell)
import qualified Habitica.Api as Api (fetchPartyMembers)
import Habitica.Request (HabiticaApi, HabiticaRequestError)
import qualified Habitica.Request as HReq (responseBody)
import Habitica.Types (Member)
import Optics ((%), (^.))
import Polysemy (Sem)
import Polysemy.Error (Error)

lastCrons ::
  forall t r.
  (BotCommandC '[HabiticaApi, Error HabiticaRequestError] r, Tellable t) =>
  t ->
  Sem r ()
lastCrons = commandWithError lastCrons'

lastCrons' :: (BotCommandC '[HabiticaApi] r, Tellable t) => t -> Sem r ()
lastCrons' ctx = do
  DiP.info_ @Path "Processing !lastCrons command"

  DiP.debug_ "Fetching party members from Habitica"
  members <- HReq.responseBody <$> Api.fetchPartyMembers

  DiP.debug_ $
    "Member cron times: "
      <> fromString
        ( show
            ( map
                ( \m ->
                    ( m ^. #auth % #local % #username,
                      m ^. #auth % #timestamps % #loggedin
                    )
                )
                members
            )
        )

  DiP.debug_ "Sending cron times message to Discord"
  tell ctx $ mkLastCronsMessage members

mkLastCronsMessage :: [Member] -> Text
mkLastCronsMessage members =
  codeblock $
    mconcat $
      intersperse "\n" $
        header "Party members last cronned at the following times (UTC):"
          : map memberToCronText members
  where
    fmtDate :: UTCTime -> Text
    fmtDate = yellow . toText . Time.formatTime Time.defaultTimeLocale "%b %d %Y"

    fmtTime :: UTCTime -> Text
    fmtTime = yellow . toText . Time.formatTime Time.defaultTimeLocale "%H:%M:%S"

    lenLongestName = maxTextLenBy (^. #auth % #local % #username) members

    memberToCronText :: Member -> Text
    memberToCronText member =
      T.justifyLeft lenLongestName ' ' (member ^. #auth % #local % #username)
        <> " last cronned at "
        <> fmtTime lastCron
        <> " on "
        <> fmtDate lastCron
      where
        lastCron = member ^. #auth % #timestamps % #loggedin
