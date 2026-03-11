{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.MemInfo.SysInfo
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides data types that

- define memory reports (cf @'ReportBud'@) and

- provide info about the system where the report will run (cf @'KernelVersion'@,
@'SwapFlaw'@ and @'RamFlaw'@).

along with functions that use these types.
-}
module System.MemInfo.SysInfo (
  -- * define reports
  ReportBud (..),
  mkReportBud,

  -- * indicate calculation flaws
  RamFlaw (..),
  SwapFlaw (..),
  checkForFlaws,
  fmtRamFlaws,
  fmtSwapFlaws,

  -- * system kernel version
  KernelVersion,
  parseKernelVersion,
  readKernelVersion,
  fickleSharing,
) where

import Control.Monad ((>=>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Fmt ((+|), (|+))
import System.MemInfo.Prelude


-- | Represents a version of @Linux@ kernel
type KernelVersion = (Natural, Natural, Natural)


{- | On linux kernels before smaps became available, there was no reliable way to
determine how much of a processes memory was shared

http://lkml.org/lkml/2005/7/6/250
-}
fickleSharing :: KernelVersion -> Bool
fickleSharing k = k >= (2, 6, 1) && k <= (2, 6, 9)


-- | Determines the version of the Linux kernel on the current system.
readKernelVersion :: FilePath -> IO (Maybe KernelVersion)
readKernelVersion = fmap parseKernelVersion . Text.readFile . kernelVersionPath


kernelVersionPath :: FilePath -> FilePath
kernelVersionPath root = "" +| root |+ "/sys/kernel/osrelease"


-- | Parses @Text@ into a @'KernelVersion'@
parseKernelVersion :: Text -> Maybe KernelVersion
parseKernelVersion =
  let unrecognized = Nothing
      dec' (Right (x, extra)) | Text.null extra = Just x
      dec' _ = unrecognized
      dec1st' (Right (x, _)) = Just x
      dec1st' _ = unrecognized

      dec = dec' . Text.decimal
      dec1st = dec1st' . Text.decimal
      fromSplit [x] = (,,) <$> dec x <*> pure 0 <*> pure 0
      fromSplit [x, y] = (,,) <$> dec x <*> dec1st y <*> pure 0
      fromSplit [x, y, z] = (,,) <$> dec x <*> dec y <*> dec1st z
      fromSplit _ = unrecognized
   in fromSplit . Text.split (== '.')


-- | Gathers the inputs needed to generate a memory usage report
data ReportBud = ReportBud
  { rbPids :: !(NonEmpty ProcessID)
  , rbKernel :: !KernelVersion
  , rbHasPss :: !Bool
  , rbHasSwapPss :: !Bool
  , rbHasSmaps :: !Bool
  , rbRamFlaws :: !(Maybe RamFlaw)
  , rbSwapFlaws :: !(Maybe SwapFlaw)
  , rbProcRoot :: !FilePath
  }
  deriving (Eq, Show)


-- | Describes inaccuracies in the RAM calculation
data RamFlaw
  = -- | no shared mem is reported
    NoSharedMem
  | -- | some shared mem not reported
    SomeSharedMem
  | -- | accurate only considering each process in isolation
    ExactForIsolatedMem
  deriving (Eq, Show, Ord)


-- | Provide @Text@ that explains the 'RamFlaw'
fmtRamFlaws :: RamFlaw -> Text
fmtRamFlaws NoSharedMem =
  Text.unlines
    [ "shared memory is not reported by this system."
    , "Values reported will be too large, and totals are not reported"
    ]
fmtRamFlaws SomeSharedMem =
  Text.unlines
    [ "shared memory is not reported accurately by this system."
    , "Values reported could be too large, and totals are not reported"
    ]
fmtRamFlaws ExactForIsolatedMem =
  Text.unlines
    [ "shared memory is slightly over-estimated by this system"
    , "for each program, so totals are not reported."
    ]


-- | Describes inaccuracies in the swap measurement
data SwapFlaw
  = -- | not available
    NoSwap
  | -- | accurate only considering each process in isolation
    ExactForIsolatedSwap
  deriving (Eq, Show, Ord)


-- | Provide @Text@ that explains the 'SwapFlaw'
fmtSwapFlaws :: SwapFlaw -> Text
fmtSwapFlaws NoSwap = "swap is not reported by this system."
fmtSwapFlaws ExactForIsolatedSwap =
  Text.unlines
    [ "swap is over-estimated by this system"
    , "for each program, so totals are not reported."
    ]


{- | Examine the target system for @'RamFlaw's@ and @'SwapFlaw's@, and update
@bud@ reflect the findings.
-}
checkForFlaws :: ReportBud -> IO ReportBud
checkForFlaws bud = do
  let memInfoPath = meminfoPathOf (rbProcRoot bud)
      ReportBud
        { rbHasPss = hasPss
        , rbHasSmaps = hasSmaps
        , rbHasSwapPss = hasSwapPss
        , rbKernel = version
        } = bud
  (rbRamFlaws, rbSwapFlaws) <- case version of
    (2, 4, _patch) -> do
      let hasInact = Text.isInfixOf "Inact_" <$> readUtf8Text memInfoPath
          andHasInact x = if x then hasInact else pure False
          memInfoMem x = if x then SomeSharedMem else ExactForIsolatedMem
          mkFlawPair x = (Just $ memInfoMem x, Just NoSwap)
          isInactPresent = doesFileExist >=> andHasInact
      mkFlawPair <$> isInactPresent memInfoPath
    (2, 6, _patch) -> do
      pure $
        if
          | hasSmaps && hasPss -> (Nothing, Just ExactForIsolatedSwap)
          | hasSmaps -> (Just ExactForIsolatedMem, Just ExactForIsolatedSwap)
          | fickleSharing version -> (Just NoSharedMem, Just NoSwap)
          | otherwise -> (Just SomeSharedMem, Just NoSwap)
    (major, _minor, _patch) | major > 2 && hasSmaps -> do
      pure (Nothing, if hasSwapPss then Nothing else Just ExactForIsolatedSwap)
    _other -> pure (Just ExactForIsolatedMem, Just NoSwap)
  pure $ bud {rbRamFlaws, rbSwapFlaws}


{- | Construct a @ReportBud@ from some @ProcessIDs@

Generates values for the other fields by inspecting the system

The result is @Nothing@ when either

- the process root is not accessible
- the @KernelVersion@ cannot be determined
-}
mkReportBud :: FilePath -> NonEmpty ProcessID -> IO (Maybe ReportBud)
mkReportBud rbProcRoot rbPids = do
  let firstPid = NE.head rbPids
      smapsPath = pidPath rbProcRoot "smaps" firstPid
      hasPss = Text.isInfixOf "Pss:"
      hasSwapPss = Text.isInfixOf "SwapPss:"
      memtypes x = (hasPss x, hasSwapPss x)
  rootAccessible <- canAccessRoot rbProcRoot
  if not rootAccessible
    then pure Nothing
    else do
      rbHasSmaps <- doesFileExist smapsPath
      (rbHasPss, rbHasSwapPss) <-
        if rbHasSmaps
          then memtypes <$> readUtf8Text smapsPath
          else pure (False, False)
      readKernelVersion rbProcRoot >>= \case
        Nothing -> pure Nothing
        Just rbKernel ->
          fmap Just $
            checkForFlaws $
              ReportBud
                { rbPids
                , rbKernel
                , rbHasPss
                , rbHasSwapPss
                , rbHasSmaps
                , rbRamFlaws = Nothing
                , rbSwapFlaws = Nothing
                , rbProcRoot
                }


pidPath :: FilePath -> FilePath -> ProcessID -> FilePath
pidPath root base pid = "" +| root |+ "/" +| toInteger pid |+ "/" +| base |+ ""


meminfoPathOf :: FilePath -> FilePath
meminfoPathOf root = "" +| root |+ "/meminfo"


canAccessRoot :: FilePath -> IO Bool
canAccessRoot root = do
  doesRootExist <- doesDirectoryExist root
  if not doesRootExist
    then pure False
    else do
      p <- getPermissions root
      pure $ readable p && searchable p
