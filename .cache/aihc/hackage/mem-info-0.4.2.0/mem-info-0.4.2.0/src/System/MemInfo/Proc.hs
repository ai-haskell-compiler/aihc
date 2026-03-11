{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.MemInfo.Proc
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides types that model data from files in the @Linux proc
filesystem@ that track memory usage, combinators for parsing the files contents,
and for grouping the results.
-}
module System.MemInfo.Proc (
  -- * Combine process memory metrics
  MemUsage (..),
  amass,

  -- * Parse process memory metrics
  ProcUsage (..),
  parseFromSmap,
  parseFromStatm,

  -- * Parse \/proc\/\<pid\>\/exe
  ExeInfo (..),
  parseExeInfo,

  -- * Parse \/proc\/<pid\>\/status
  parseStatusInfo,
  StatusInfo (..),
  BadStatus (..),
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Validity (Validity (..), check, delve)
import Data.Validity.Text ()
import GHC.Generics (Generic)
import System.MemInfo.Prelude
import System.MemInfo.SysInfo (KernelVersion, fickleSharing)


-- | Represents the information about a process obtained from /proc/<pid>/status
data StatusInfo = StatusInfo
  { siName :: !Text
  , siParent :: !ProcessID
  }
  deriving (Eq, Show, Generic)


instance Validity StatusInfo where
  validate StatusInfo {siName, siParent} =
    let name' = Text.strip siName
        noNewlines = not $ Text.any (== '\n') siName
        nameOk = noNewlines && Text.length name' > 0 && siName == name'
     in mconcat
          [ check nameOk "the process name"
          , delve "the process ID" $ toInteger siParent
          ]


-- | Indicates why @'parseStatusInfo'@ failed
data BadStatus
  = NoCmd
  | NoParent
  deriving (Eq, Show)


-- | Parses the content of \/proc\/\<pid\>\/status into a @'StatusInfo'@
parseStatusInfo :: Text -> Either BadStatus StatusInfo
parseStatusInfo content =
  let
    statusLines = Text.lines content
    parseLine key l = Text.strip <$> Text.stripPrefix (key <> ":") l
    mkStep prefix acc l = case acc of
      Nothing -> parseLine prefix l
      found -> found
    name = maybe (Left NoCmd) Right name'
    name' = foldl' (mkStep "Name") Nothing statusLines
    ppidTxt = foldl' (mkStep "PPid") Nothing statusLines
    parsePpid = readMaybe . Text.unpack
    ppId = maybe (Left NoParent) Right (ppidTxt >>= parsePpid)
   in
    StatusInfo <$> name <*> ppId


-- | Parses the target of \/proc\/\<pid\>\/exe into a @'ExeInfo'@
parseExeInfo :: Text -> ExeInfo
parseExeInfo x =
  let eiTarget = takeTillNull x
      eiDeleted = delEnd `Text.isSuffixOf` eiTarget
      withoutDeleted = Text.replace delEnd "" eiTarget
      eiOriginal = if eiDeleted then withoutDeleted else eiTarget
      takeTillNull = Text.takeWhile (not . isNull)
   in ExeInfo {eiDeleted, eiOriginal, eiTarget}


delEnd :: Text
delEnd = " (deleted)"


-- | Represents the information about a process obtained from \/proc\/\<pid\>\/exe
data ExeInfo = ExeInfo
  { eiTarget :: !Text
  -- ^ the path that the link \/proc\/\<pid\>\/exe resolves to
  , eiOriginal :: !Text
  -- ^ a sanitized form of eiTarget; it removes the / (deleted)/ suffix
  , eiDeleted :: !Bool
  -- ^ does eiTarget end with /(deleted)/?
  }
  deriving (Eq, Show, Generic)


instance Validity ExeInfo where
  validate ei | eiDeleted ei = check (eiOriginal ei <> delEnd == eiTarget ei) "target is actually deleted"
  validate ei = check (eiOriginal ei == eiTarget ei) "target is not deleted"


-- | Combine @'ProcUsage'@, grouping them by the effective program name
amass ::
  (Ord a) =>
  Bool ->
  [(a, ProcUsage)] ->
  Map a MemUsage
amass hasPss = Map.map (fromSubTotal hasPss) . foldl' (incrSubTotals hasPss) mempty


fromSubTotal :: Bool -> SubTotal -> MemUsage
fromSubTotal hasPss st =
  let reducedPrivate = stPrivate st `div` stCount st
      areThreads = threadsNotProcs st
      reducedShared = stShared st `div` stCount st
      newPrivate = if areThreads then reducedPrivate else stPrivate st
      newShared = if areThreads && hasPss then reducedShared else stShared st
   in MemUsage
        { muShared = newShared + stSharedHuge st
        , muPrivate = newPrivate + newShared + stSharedHuge st
        , muSwap = stSwap st
        , muCount = stCount st
        }


-- | Represents the measured memory usage of a program
data MemUsage = MemUsage
  { muShared :: !Int
  -- ^ the total shared memory in use
  , muPrivate :: !Int
  -- ^ the total private memory in use
  , muCount :: !Int
  -- ^ the number of processes running as the program
  , muSwap :: !Int
  -- ^ the total swap memory in use
  }
  deriving (Eq, Show)


incrSubTotals ::
  (Ord a) =>
  Bool ->
  Map a SubTotal ->
  (a, ProcUsage) ->
  Map a SubTotal
incrSubTotals hasPss acc (cmd, mem) =
  let combinePrivate next prev | hasPss = next + prev
      combinePrivate next prev = max next prev
      nextSt =
        SubTotal
          { stShared = puShared mem
          , stSharedHuge = puSharedHuge mem
          , stCount = 1
          , stPrivate = puPrivate mem
          , stSwap = puSwap mem
          , stMemIds = Set.singleton $ puMemId mem
          }
      update' next prev =
        prev
          { stShared = stShared next + stShared prev
          , stSharedHuge = max (stSharedHuge next) (stSharedHuge prev)
          , stPrivate = combinePrivate (stPrivate next) (stPrivate prev)
          , stCount = stCount next + stCount prev
          , stSwap = stSwap next + stSwap prev
          , stMemIds = Set.union (stMemIds next) (stMemIds prev)
          }
   in Map.insertWith update' cmd nextSt acc


data SubTotal = SubTotal
  { stShared :: !Int
  , stSharedHuge :: !Int
  , stPrivate :: !Int
  , stCount :: !Int
  , stSwap :: !Int
  , stMemIds :: !(Set Int)
  }
  deriving (Eq, Show)


-- If a process is invoked with clone using flags CLONE_VM and not CLONE_THREAD
-- it will share the same memory space as it's parent; this needs to accounted
-- for
--
-- This is detected by computing the memId has the hash of lines for the proc
-- read from its smaps file.
threadsNotProcs :: SubTotal -> Bool
threadsNotProcs cs = Set.size (stMemIds cs) == 1 && stCount cs > 1


-- | Represents the memory metrics for a single process
data ProcUsage = ProcUsage
  { puPrivate :: !Int
  , puShared :: !Int
  , puSharedHuge :: !Int
  , puSwap :: !Int
  , puMemId :: !Int
  }
  deriving (Eq, Show)


-- value used as page size when @MemStat@ is calcuated from statm
pageSizeKiB :: Int
pageSizeKiB = 4


-- | Parse @'ProcUsage'@ from the contents of \/proc\/\<pid\>\/statm
parseFromStatm :: KernelVersion -> Text -> Maybe ProcUsage
parseFromStatm version content =
  let
    parseWord w (Just acc) = (\x -> Just (x : acc)) =<< readMaybe (Text.unpack w)
    parseWord _ Nothing = Nothing
    parseMetrics = foldr parseWord (Just mempty)
    withMemId = ppZero {puMemId = hash content}
    fromRss rss _shared
      | fickleSharing version = withMemId {puPrivate = rss * pageSizeKiB}
    fromRss rss shared =
      withMemId
        { puShared = shared * pageSizeKiB
        , puPrivate = (rss - shared) * pageSizeKiB
        }
    fromRss' (_size : rss : shared : _xs) = Just $ fromRss rss shared
    fromRss' _ = Nothing
   in
    parseMetrics (Text.words content) >>= fromRss'


ppZero :: ProcUsage
ppZero =
  ProcUsage
    { puPrivate = 0
    , puShared = 0
    , puSharedHuge = 0
    , puSwap = 0
    , puMemId = 0
    }


-- | Parse @'ProcUsage'@ from the contents of \/proc\/\<pid\>\/smap
parseFromSmap :: Text -> ProcUsage
parseFromSmap = fromSmap . parseSmapStats


parseSmapStats :: Text -> SmapStats
parseSmapStats content =
  let noMemId = foldl' incrSmapStats ssZero $ Text.lines content
   in noMemId {ssMemId = hash content}


fromSmap :: SmapStats -> ProcUsage
fromSmap ss =
  let pssTweak = ssPssCount ss `div` 2 -- add ~0.5 per line to counter truncation
      pssShared = ssPss ss + pssTweak - ssPrivate ss
   in ProcUsage
        { puSwap = if ssHasSwapPss ss then ssSwapPss ss else ssSwap ss
        , puShared = if ssHasPss ss then pssShared else ssShared ss
        , puSharedHuge = ssSharedHuge ss
        , puPrivate = ssPrivate ss + ssPrivateHuge ss
        , puMemId = ssMemId ss
        }


-- | Represents per-process data read from \/proc\/\<pid\>\/smap
data SmapStats = SmapStats
  { ssPss :: !Int
  , ssPssCount :: !Int
  , ssSwap :: !Int
  , ssSwapPss :: !Int
  , ssPrivate :: !Int
  , ssPrivateHuge :: !Int
  , ssSharedHuge :: !Int
  , ssShared :: !Int
  , ssMemId :: !Int
  , ssHasPss :: !Bool
  , ssHasSwapPss :: !Bool
  }
  deriving (Eq, Show)


ssZero :: SmapStats
ssZero =
  SmapStats
    { ssPss = 0
    , ssPssCount = 0
    , ssSwap = 0
    , ssSwapPss = 0
    , ssPrivate = 0
    , ssPrivateHuge = 0
    , ssSharedHuge = 0
    , ssShared = 0
    , ssHasSwapPss = False
    , ssHasPss = False
    , ssMemId = 0
    }


-- Q: is it worth the dependency to replace this with lens from a lens package ?
incrPss
  , incrSwap
  , incrSwapPss
  , incrPrivate
  , incrPrivateHuge
  , incrShared
  , incrSharedHuge ::
    SmapStats -> Maybe Int -> SmapStats
incrPss ms = maybe ms $ \n -> ms {ssPss = n + ssPss ms}
incrSwap ms = maybe ms $ \n -> ms {ssSwap = n + ssSwap ms}
incrSwapPss ms = maybe ms $ \n -> ms {ssSwapPss = n + ssSwapPss ms}
incrPrivate ms = maybe ms $ \n -> ms {ssPrivate = n + ssPrivate ms}
incrShared ms = maybe ms $ \n -> ms {ssShared = n + ssShared ms}
incrPrivateHuge ms = maybe ms $ \n -> ms {ssPrivateHuge = n + ssPrivateHuge ms}
incrSharedHuge ms = maybe ms $ \n -> ms {ssSharedHuge = n + ssSharedHuge ms}


incrSmapStats :: SmapStats -> Text -> SmapStats
incrSmapStats acc l =
  if
    | Text.isPrefixOf "Private_Hugetlb:" l -> incrPrivateHuge acc $ smapValMb l
    | Text.isPrefixOf "Shared_Hugetlb:" l -> incrSharedHuge acc $ smapValMb l
    | Text.isPrefixOf "Shared" l -> incrShared acc $ smapValMb l
    | Text.isPrefixOf "Private" l -> incrPrivate acc $ smapValMb l
    | Text.isPrefixOf "Pss:" l ->
        let acc' = acc {ssHasPss = True, ssPssCount = 1 + ssPssCount acc}
         in incrPss acc' $ smapValMb l
    | Text.isPrefixOf "Swap:" l -> incrSwap acc $ smapValMb l
    | Text.isPrefixOf "SwapPss:" l -> incrSwapPss (acc {ssHasSwapPss = True}) $ smapValMb l
    | otherwise -> acc


smapValMb :: (Read a) => Text -> Maybe a
smapValMb l =
  let memWords = Text.words l
      readVal (_ : x : _) = readMaybe $ Text.unpack x
      readVal _ = Nothing
   in readVal memWords
