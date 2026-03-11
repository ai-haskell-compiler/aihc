module Trace.Hpc.Lcov (
  generateLcovFromTix,
  writeReport,
  FileInfo,
) where

import Control.Arrow ((&&&))
import Data.List (intercalate, maximumBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Trace.Hpc.Mix (BoxLabel (..), MixEntry)
import Trace.Hpc.Tix (TixModule (..), tixModuleName, tixModuleTixs)
import Trace.Hpc.Util (HpcPos, fromHpcPos, toHash)

import Trace.Hpc.Lcov.Report

-- | Path to source file and entries from the corresponding .mix file.
type FileInfo = (FilePath, [MixEntry])

-- | Generate LCOV format from HPC coverage data.
generateLcovFromTix ::
  [(String, FileInfo)]
  -- ^ Mapping from module name to file info
  -> [TixModule]
  -> LcovReport
generateLcovFromTix moduleToMix = LcovReport . map mkFileReport . mergeTixModules
  where
    mkFileReport tixModule =
      let tickCounts = tixModuleTixs tixModule
          moduleName = tixModuleName tixModule
          (fileLoc, mixEntries) =
            fromMaybe
              (error $ "Could not find .mix file for: " ++ moduleName)
              $ moduleName `lookup` moduleToMix
          overTixMix f = catMaybes $ zipWith f tickCounts mixEntries
       in FileReport
            { fileReportLocation = fileLoc
            , fileReportFunctions = overTixMix parseFunctionReport
            , fileReportBranches = mergeBranchReports $ overTixMix parseBranchReport
            , fileReportLines = mergeLineReports $ overTixMix parseLineReport
            }

-- | Merge all tix modules representing the same module.
--
--  If tix modules are duplicated, we are treating them as being hit in different test suites, so all
--  tick counts should be added together.
mergeTixModules :: [TixModule] -> [TixModule]
mergeTixModules = Map.elems . Map.fromListWith mergeTixs . map (tixModuleName &&& id)
  where
    mergeTixs (TixModule moduleName hash len ticks1) (TixModule _ _ _ ticks2) =
      TixModule moduleName hash len $ zipWith (+) ticks1 ticks2

parseFunctionReport :: Integer -> MixEntry -> Maybe FunctionReport
parseFunctionReport tickCount (hpcPos, boxLabel) = mkFunctionReport <$> mFunctionName
  where
    mkFunctionReport names =
      FunctionReport
        { functionReportLine = hpcPosLine hpcPos
        , functionReportName = intercalate "$" names
        , functionReportHits = tickCount
        }

    mFunctionName = case boxLabel of
      TopLevelBox names -> Just names
      LocalBox names -> Just names
      _ -> Nothing

parseBranchReport :: Integer -> MixEntry -> Maybe (HpcPos, (Integer, Integer))
parseBranchReport tickCount (hpcPos, boxLabel) = case boxLabel of
  BinBox _ isTrue ->
    let branchHits = if isTrue then (tickCount, 0) else (0, tickCount)
     in Just (hpcPos, branchHits)
  _ -> Nothing

mergeBranchReports :: [(HpcPos, (Integer, Integer))] -> [BranchReport]
mergeBranchReports = map mkBranchReport . Map.toList . Map.fromListWith addPairs
  where
    mkBranchReport (hpcPos, (trueHits, falseHits)) =
      BranchReport
        { branchReportLine = hpcPosLine hpcPos
        , branchReportHash = toHash hpcPos
        , branchReportTrueHits = trueHits
        , branchReportFalseHits = falseHits
        }

    addPairs (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

parseLineReport :: Integer -> MixEntry -> Maybe LineReport
parseLineReport tickCount (hpcPos, boxLabel) = case boxLabel of
  ExpBox _ ->
    Just
      LineReport
        { lineReportLine = hpcPosLine hpcPos
        , lineReportHits = tickCount
        }
  _ -> Nothing

mergeLineReports :: [LineReport] -> [LineReport]
mergeLineReports = Map.elems . Map.fromListWith (maxBy lineReportHits) . map (lineReportLine &&& id)

{- Utilities -}

hpcPosLine :: HpcPos -> Int
hpcPosLine = (\(startLine, _, _, _) -> startLine) . fromHpcPos

maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f a b = maximumBy (comparing f) [a, b]
