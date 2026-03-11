{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lcov where

import Data.Bifunctor (first)
import Data.List (sortOn)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Trace.Hpc.Mix (BoxLabel (..), CondBox (..))
import Trace.Hpc.Tix (TixModule (..))
import Trace.Hpc.Util (Hash, HpcPos, toHash, toHpcPos)

import Trace.Hpc.Lcov
import Trace.Hpc.Lcov.Report

test_generate_lcov_top_level :: TestTree
test_generate_lcov_top_level =
  testCase "generateLcovFromTix FunctionReport TopLevelBox" $
    let report =
          generateLcovFromTixMix
            [ TixMix
                "Test"
                "Test.hs"
                [ TixMixEntry 1 (TopLevelBox ["foo"]) 0
                , TixMixEntry 2 (TopLevelBox ["bar"]) 10
                ]
            ]
     in fromReport report
          @?= [ FileReport
                  "Test.hs"
                  [ FunctionReport 1 "foo" 0
                  , FunctionReport 2 "bar" 10
                  ]
                  []
                  []
              ]

test_generate_lcov_local :: TestTree
test_generate_lcov_local =
  testCase "generateLcovFromTix FunctionReport LocalBox" $
    let report =
          generateLcovFromTixMix
            [ TixMix
                "Test"
                "Test.hs"
                [ TixMixEntry 1 (LocalBox ["foo", "bar"]) 0
                , TixMixEntry 2 (LocalBox ["foo", "baz"]) 10
                ]
            ]
     in fromReport report
          @?= [ FileReport
                  "Test.hs"
                  [ FunctionReport 1 "foo$bar" 0
                  , FunctionReport 2 "foo$baz" 10
                  ]
                  []
                  []
              ]

test_generate_lcov_branch :: TestTree
test_generate_lcov_branch =
  testCase "generateLcovFromTix BranchReport" $
    let report =
          generateLcovFromTixMix
            [ TixMix
                "Test"
                "Test.hs"
                [ TixMixEntry 1 (BinBox GuardBinBox True) 0
                , TixMixEntry 1 (BinBox GuardBinBox False) 10
                ]
            ]
     in fromReport report
          @?= [ FileReport
                  "Test.hs"
                  []
                  [ BranchReport 1 (getHash 1) 0 10
                  ]
                  []
              ]

test_generate_lcov_line :: TestTree
test_generate_lcov_line =
  testCase "generateLcovFromTix LineReport" $
    let report =
          generateLcovFromTixMix
            [ TixMix
                "Test"
                "Test.hs"
                [ TixMixEntry 1 (ExpBox True) 0
                , TixMixEntry 2 (ExpBox False) 10
                , TixMixEntry 2 (ExpBox True) 4
                , TixMixEntry 3 (ExpBox False) 0
                , TixMixEntry 3 (ExpBox True) 2
                ]
            ]
     in fromReport report
          @?= [ FileReport
                  "Test.hs"
                  []
                  []
                  [ LineReport 1 0
                  , -- these take the highest hit count for the line
                    LineReport 2 10
                  , LineReport 3 2
                  ]
              ]

test_generate_lcov_merge_tixs :: TestTree
test_generate_lcov_merge_tixs =
  testCase "generateLcovFromTix merge .tix files" $
    let report =
          generateLcovFromTix
            [ mkModuleToMix
                "Test"
                "Test.hs"
                [ (1, TopLevelBox ["foo"])
                , (2, LocalBox ["foo", "bar"])
                , (3, ExpBox True)
                , (3, BinBox CondBinBox True)
                , (3, BinBox CondBinBox False)
                ]
            ]
            [ mkTix "Test" [1, 2, 3, 4, 0]
            , mkTix "Test" [2, 3, 4, 5, 0]
            ]
     in fromReport report
          @?= [ FileReport
                  "Test.hs"
                  [ FunctionReport 1 "foo" 3
                  , FunctionReport 2 "foo$bar" 5
                  ]
                  [ BranchReport 3 (getHash 3) 9 0
                  ]
                  [ LineReport 3 7
                  ]
              ]

{- Helpers -}

mkTix :: String -> [Integer] -> TixModule
mkTix moduleName ticks = TixModule moduleName 0 (length ticks) ticks

mkModuleToMix :: String -> FilePath -> [(Int, BoxLabel)] -> (String, FileInfo)
mkModuleToMix moduleName filePath mixEntries = (moduleName, (filePath, mixs))
  where
    mixs = map (first getHpcPos) mixEntries

data TixMix = TixMix
  { tixMixModule :: String
  , tixMixFilePath :: FilePath
  , tixMixEntries :: [TixMixEntry]
  }

data TixMixEntry = TixMixEntry
  { tixMixEntryLine :: Int
  , tixMixBoxLabel :: BoxLabel
  , tixMixEntryTicks :: Integer
  }

generateLcovFromTixMix :: [TixMix] -> LcovReport
generateLcovFromTixMix = uncurry generateLcovFromTix . unzip . map fromTixMix
  where
    fromTixMix TixMix{..} =
      let toMixEntry (TixMixEntry line boxLabel _) = (line, boxLabel)
          moduleToMix = mkModuleToMix tixMixModule tixMixFilePath $ map toMixEntry tixMixEntries
          tix = mkTix tixMixModule $ map tixMixEntryTicks tixMixEntries
       in (moduleToMix, tix)

fromReport :: LcovReport -> [FileReport]
fromReport (LcovReport fileReports) = sortOn fileReportLocation fileReports

getHpcPos :: Int -> HpcPos
getHpcPos line = toHpcPos (line, 0, 0, 0)

getHash :: Int -> Hash
getHash = toHash . getHpcPos
