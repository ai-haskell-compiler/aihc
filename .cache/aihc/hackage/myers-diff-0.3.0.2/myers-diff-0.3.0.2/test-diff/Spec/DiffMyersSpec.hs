{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Spec.DiffMyersSpec (spec) where

import Data.Diff.Myers
import Data.Diff.Types
import Data.Text as T
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators


spec :: TopSpec
spec = describe "DiffMyers" $ do
  describe "Single-line cases" $ do
    it "simple insertion" $ do
      diffTextsToChangeEvents "ab" "abc" `shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 2)) "c"])

    it "simple deletion" $ do
      diffTextsToChangeEvents "abc" "ab" `shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 3)) ""])

  describe "QuickCheck" $ introduceQuickCheck $ modifyMaxSuccess (const 10000) $ do
    prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
    prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to


verifyDiff :: Text -> Text -> Bool
verifyDiff from to = applyChangesText change from == to
  where
    change = diffTextsToChangeEvents from to

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
