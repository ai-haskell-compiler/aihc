{-# LANGUAGE OverloadedStrings #-}

module Test.TcStackageProgress
  ( tcStackageProgressTests,
  )
where

import Data.Map.Strict qualified as Map
import ResolveStackageProgress (PackageStatus (..))
import TcStackageProgress (PackageCounts (..), summarizePackageStatuses)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tcStackageProgressTests :: TestTree
tcStackageProgressTests =
  testGroup
    "tc stackage progress"
    [ testCase "counts typechecked, failed, and skipped packages" test_countsPackageStatuses
    ]

test_countsPackageStatuses :: IO ()
test_countsPackageStatuses = do
  let statuses =
        Map.fromList
          [ ("base", PkgSuccess Map.empty),
            ("containers", PkgFailed "type error"),
            ("text", PkgSkipped)
          ]
  assertEqual
    "counts"
    PackageCounts
      { countTypechecked = 1,
        countFailed = 1,
        countSkipped = 1,
        countTotal = 3
      }
    (summarizePackageStatuses statuses)
