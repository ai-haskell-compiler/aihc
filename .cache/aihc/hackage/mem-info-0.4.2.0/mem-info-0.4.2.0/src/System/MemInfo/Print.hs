{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : System.MemInfo.Print
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides functions that format the output of the __printmem__ command
-}
module System.MemInfo.Print (
  AsCmdName (asCmdName),
  fmtMem,
  fmtAsHeader,
  fmtOverall,
  fmtMemUsage,
  styleOutput,
) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Fmt (
  build,
  fixedF,
  padBothF,
  padLeftF,
  padRightF,
  (+|),
  (+||),
  (|+),
  (|++|),
  (||+),
 )
import System.MemInfo.Choices (
  Mem (..),
  Power (..),
  Style (..),
 )
import System.MemInfo.Prelude
import System.MemInfo.Proc (MemUsage (..))


-- | Generate the output for a given report using the specified style
styleOutput :: (AsCmdName a) => Bool -> Style -> Bool -> [(a, MemUsage)] -> [Text]
styleOutput showSwap style isAccurate = outputOf isAccurate (printStyle style showSwap)


{- | Generates the text of a row displaying the metrics for a single command in
the memory report
-}
fmtMemUsage :: (AsCmdName a) => Bool -> a -> MemUsage -> Text
fmtMemUsage showSwap name ct =
  let
    padl = padLeftF columnWidth ' ' . fmtMemKb
    private = padl $ muPrivate ct - muShared ct
    shared = padl $ muShared ct
    all' = padl $ muPrivate ct
    swap' = padl $ muSwap ct
    name' = cmdWithCount name $ muCount ct
    ram = private |+ " + " +| shared |+ " = " +| all'
    numbers = if showSwap then ram +| swap' else ram
   in
    numbers |+ "\t" +| name' |+ ""


fmtMemUsageCsv :: (AsCmdName a) => Bool -> a -> MemUsage -> Text
fmtMemUsageCsv showSwap name ct =
  let
    private = build $ muPrivate ct - muShared ct
    shared = build $ muShared ct
    all' = build $ muPrivate ct
    swap' = build $ muSwap ct
    name' = cmdWithCount name $ muCount ct
    ram = private |+ "," +| shared |+ "," +| all' |+ ","
    numbers = if showSwap then ram +| swap' |+ "," else ram
   in
    numbers +| name' |+ ""


-- | Generates the text showing the overall memory in the memory report
fmtOverall :: Bool -> (Int, Int) -> Text
fmtOverall showSwap (private, swap) =
  let
    rimLength = if showSwap then 46 else 36
    gapLength = 26
    top = Text.replicate rimLength "-"
    gap = Text.replicate gapLength " "
    bottom = Text.replicate rimLength "="
    padl = padLeftF columnWidth ' ' . fmtMemKb
    withSwap = "" +| gap |++| padl private |++| padl swap |+ ""
    noSwap = "" +| gap |++| padl private |+ ""
    out = if showSwap then withSwap else noSwap
   in
    Text.unlines [top, out, bottom]


fmtMemKb :: Int -> Text
fmtMemKb = fmtMem . Mem Ki . fromIntegral


columnWidth :: Int
columnWidth = 10


doFmt :: Power -> Double -> Text
doFmt =
  let doFmt' p x = "" +| fixedF 1 x |+ " " +|| p ||+ "B"
      go p x | p == maxBound = doFmt' p x
      go p x | x >= 1024 = doFmt (succ p) (x / 1024.0)
      go p x = doFmt' p x
   in go


fmtMem :: Mem -> Text
fmtMem (Mem p x) = doFmt p $ realToFrac x


hdrPrivate, hdrShared, hdrRamUsed, hdrSwapUsed, hdrProgram, hdrCount, hdrPid :: Text
hdrPrivate = "Private"
hdrShared = "Shared"
hdrRamUsed = "RAM Used"
hdrSwapUsed = "Swap Used"
hdrProgram = "Program"
hdrCount = "(# processes)"
hdrPid = "[pid]"


-- | Generates the text of the printed header of the memory report
fmtAsHeader :: Bool -> Bool -> Text
fmtAsHeader hasPid showSwap =
  let
    padb = padBothF columnWidth ' '
    padr = padRightF columnWidth ' '
    padl = padLeftF columnWidth ' '
    private = padb hdrPrivate
    shared = padb hdrShared
    all' = padl hdrRamUsed
    nameExt = if hasPid then hdrPid else hdrCount
    name' = padr $ hdrProgram <> " " <> nameExt
    swap' = padl hdrSwapUsed
    ram = private |+ " + " +| shared |+ " = " +| all'
    numbers = if showSwap then ram +| swap' else ram
   in
    numbers |+ "\t" +| name' |+ ""


-- | Generates the text of the printed header of the memory report
fmtAsHeaderCsv :: Bool -> Bool -> Text
fmtAsHeaderCsv hasPid showSwap =
  let
    private = build hdrPrivate
    shared = build hdrShared
    all' = build hdrRamUsed
    nameExt = if hasPid then hdrPid else hdrCount
    name' = build $ hdrProgram <> " " <> nameExt
    swap' = build hdrSwapUsed
    ram = private |+ "," +| shared |+ "," +| all' |+ ","
    numbers = if showSwap then ram +| swap' |+ "," else ram
   in
    numbers +| name' |+ ""


{- | Identifies a type as a label to use to index programs in the report
output

The label is also used to group related processes under a single program
-}
class AsCmdName a where
  -- Convert the label to text to print in the report output
  asCmdName :: a -> Text


  -- Add a count of processes using that label to the label
  cmdWithCount :: a -> Int -> Text


  -- Indicate if pid or process count should shown in the hdr
  hdrHasPid :: Proxy a -> Bool


instance AsCmdName Text where
  asCmdName = id
  cmdWithCount cmd count = "" +| asCmdName cmd |+ " (" +| count |+ ")"
  hdrHasPid _ = False


instance AsCmdName (ProcessID, Text) where
  asCmdName (pid, name) = "" +| name |+ " [" +| toInteger pid |+ "]"
  cmdWithCount cmd _count = "" +| asCmdName cmd |+ ""
  hdrHasPid _ = True


overallTotals :: [MemUsage] -> (Int, Int)
overallTotals cts =
  let step (private, swap) ct = (private + muPrivate ct, swap + muSwap ct)
   in foldl' step (0, 0) cts


data Printers a = Printers
  { psUsage :: a -> MemUsage -> Text
  , psHeader :: Text
  , psOverall :: (Int, Int) -> Maybe Text
  }


printStyle :: forall a. (AsCmdName a) => Style -> Bool -> Printers a
printStyle style showSwap =
  let usageFmt Normal = fmtMemUsage
      usageFmt Csv = fmtMemUsageCsv
      headerFmt Normal = fmtAsHeader (hdrHasPid @a Proxy)
      headerFmt Csv = fmtAsHeaderCsv (hdrHasPid @a Proxy)
      overallFmt Normal x = Just $ fmtOverall showSwap x
      overallFmt Csv _ = Nothing
   in Printers
        { psUsage = usageFmt style showSwap
        , psOverall = overallFmt style
        , psHeader = headerFmt style showSwap
        }


outputOf :: (AsCmdName a) => Bool -> Printers a -> [(a, MemUsage)] -> [Text]
outputOf isAccurate style usages =
  let Printers {psUsage, psHeader, psOverall} = style
      overall = psOverall $ overallTotals $ map snd usages
      headerAndRows = [psHeader] <> map (uncurry psUsage) usages
   in case overall of
        Nothing -> headerAndRows
        Just _ | not isAccurate -> headerAndRows
        Just o -> headerAndRows <> [o]
