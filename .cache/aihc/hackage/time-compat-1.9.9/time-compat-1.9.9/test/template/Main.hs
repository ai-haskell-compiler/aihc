{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Time.Clock.Compat
import qualified Language.Haskell.TH.Syntax as TH
import Test.Tasty
import Test.Tasty.HUnit

testLift :: TestTree
testLift =
  testGroup
    "Lift instances"
#if MIN_VERSION_template_haskell(2,16,0)
    [ testCase "DiffTime" $ $$(TH.liftTyped (secondsToDiffTime 100)) @?= secondsToDiffTime 100,
      testCase "NominalDiffTime" $ $$(TH.liftTyped (secondsToNominalDiffTime 100)) @?= secondsToNominalDiffTime 100
    ]
#else
    []
#endif

tests :: TestTree
tests =
  testGroup
    "time-template"
    [ testLift
    ]

main :: IO ()
main = defaultMain tests
