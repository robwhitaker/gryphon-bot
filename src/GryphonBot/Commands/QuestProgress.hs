{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module GryphonBot.Commands.QuestProgress (questProgress) where

import Calamity (Tellable)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified DiPolysemy as DiP
import GryphonBot.Commands.Types (BotCommandC)
import GryphonBot.Commands.Utils
  ( codeblock,
    commandWithError,
    header,
    maxTextLenBy,
    yellow,
  )
import GryphonBot.Utils (tell)
import qualified Habitica.Api as Api
import Habitica.Request (HabiticaApi, HabiticaRequestError)
import qualified Habitica.Request as HReq
import Habitica.Types (Member, Party)
import Optics ((%), (^.))
import Polysemy (Sem)
import Polysemy.Error (Error)

isParticipatingInQuest :: Member -> Bool
isParticipatingInQuest member =
  not (member ^. #preferences % #sleep)
    && isJust (member ^. #party % #quest % #key)

getUsername :: Member -> Text
getUsername = (^. #auth % #local % #username)

data QuestType
  = Collection
  | Boss

class HasQuestProgress (questType :: QuestType) thing where
  type Progress questType thing

  getProgress :: thing -> Progress questType thing

instance HasQuestProgress 'Collection Member where
  type Progress 'Collection Member = Int

  getProgress = (^. #party % #quest % #progress % #collectedItems)

instance HasQuestProgress 'Boss Member where
  type Progress 'Boss Member = Double

  getProgress = (^. #party % #quest % #progress % #up)

instance HasQuestProgress 'Boss Party where
  type Progress 'Boss Party = Maybe Double

  getProgress = (^. #quest % #progress % #hp)

instance HasQuestProgress 'Collection Party where
  type Progress 'Collection Party = Map Text Int

  getProgress = (^. #quest % #progress % #collect)

mkBossQuestMessage :: Double -> [Member] -> Text
mkBossQuestMessage bossHp participants =
  codeblock $
    mconcat $
      intersperse "\n" $
        header ("The current quest is a " <> yellow "BOSS" <> " quest.")
          : map memberProgressText participants
            <> [ "",
                 if bossHp - pendingDamage > 0
                   then
                     "The party still needs to do "
                       <> yellow (remainingDamage <> " more damage")
                       <> " to kill the boss."
                   else yellow "No more damage" <> " is needed to kill the boss."
               ]
  where
    lenLongestName = maxTextLenBy getUsername participants

    lenLongestDamage =
      maxTextLenBy ((show :: Int -> Text) . floor . getProgress @'Boss) participants

    pendingDamage = sum $ map (getProgress @'Boss) participants

    remainingDamage = show (ceiling (bossHp - pendingDamage) :: Int)

    memberProgressText :: Member -> Text
    memberProgressText member =
      T.justifyLeft lenLongestName ' ' (getUsername member)
        <> " has "
        <> T.justifyRight lenLongestDamage ' ' (show dmg)
        <> " pending damage."
      where
        dmg :: Int = floor $ getProgress @'Boss member

mkCollectionQuestMessage :: Party -> [Member] -> Text
mkCollectionQuestMessage party participants =
  codeblock $
    mconcat $
      intersperse "\n" $
        header ("The current quest is a " <> yellow "COLLECTION" <> " quest.")
          : ["So far, the party has collected:", ""]
            <> map (uncurry partyProgressLine) (Map.assocs $ getProgress @'Collection party)
            <> [""]
            <> map memberProgressText participants
            <> [ "",
                 "In total, the party has "
                   <> yellow (show totalPending <> " pending items")
                   <> "."
               ]
  where
    lenLongestName = maxTextLenBy getUsername participants

    lenLongestPendingCollectCount =
      maxTextLenBy (show . getProgress @'Collection) participants

    lenLongestCollectedName =
      maxTextLenBy id $ Map.keys (getProgress @'Collection party)

    totalPending = sum $ map (getProgress @'Collection) participants

    memberProgressText :: Member -> Text
    memberProgressText member =
      T.justifyLeft lenLongestName ' ' (getUsername member)
        <> " has found "
        <> T.justifyRight lenLongestPendingCollectCount ' ' (show collected)
        <> " pending items."
      where
        collected = getProgress @'Collection member

    partyProgressLine :: Text -> Int -> Text
    partyProgressLine itemName itemCount =
      "- "
        <> T.justifyLeft lenLongestCollectedName ' ' itemName
        <> " x "
        <> show itemCount

questProgress ::
  forall t r. (BotCommandC '[HabiticaApi, Error HabiticaRequestError] r, Tellable t) => t -> Sem r ()
questProgress = commandWithError questProgress'

questProgress' :: (BotCommandC '[HabiticaApi] r, Tellable t) => t -> Sem r ()
questProgress' ctx = do
  DiP.info_ "Processing !questProgress command"

  DiP.debug_ "Fetching party data from Habitica"
  party <- HReq.responseBody <$> Api.fetchParty

  if not (party ^. #quest % #active)
    then do
      DiP.debug_ "No quest is in progress; sending message to Discord"
      tell ctx "No quest in progress."
    else do
      DiP.debug_ "Quest in progress; fetching party member data from Habitica"
      members <- HReq.responseBody <$> Api.fetchPartyMembers
      let participatingMembers = filter isParticipatingInQuest members
      DiP.debug_ $
        "Participating members are: "
          <> show (map getUsername participatingMembers)
      case getProgress @'Boss party of
        Just hp -> do
          DiP.debug_ $
            "Quest is a BOSS quest (found boss HP: " <> show hp <> ")"
          DiP.debug_
            "Calculating pending damage and sending stats to Discord"
          tell ctx $ mkBossQuestMessage hp participatingMembers
        Nothing -> do
          DiP.debug_ "Quest is a COLLECTION quest (could not find boss HP)"
          DiP.debug_
            "Calculating pending collected items and sending stats to Discord"
          tell ctx $ mkCollectionQuestMessage party participatingMembers
