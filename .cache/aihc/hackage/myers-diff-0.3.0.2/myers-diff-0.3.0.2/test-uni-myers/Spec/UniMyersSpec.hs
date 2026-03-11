{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.UniMyersSpec (spec) where

import Data.Diff.Types
import Data.Diff.UniMyers
import Data.Text as T
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators


spec :: TopSpec
spec = describe "uni_myers" $ do
  describe "Single-line cases" $ do
    it "simple insertion" $ do
      utilDiff "ab" "abc" `shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 2)) "c"])

    it "simple deletion" $ do
      utilDiff "abc" "ab" `shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 3)) ""])

  introduceQuickCheck $ modifyMaxSuccess (const 1000) $ do
    prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
    prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to


verifyDiff :: Text -> Text -> Bool
verifyDiff from to = applyChangesText change from == to
  where
    change = utilDiff (T.unpack from) (T.unpack to)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
