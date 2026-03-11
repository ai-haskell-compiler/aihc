{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE OverloadedLists #-}

module Spec.VectorMyersSpec (spec) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Data.Diff.Myers
import Data.Diff.Types
import Data.Text (Text)
import qualified Data.Text as T
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators
import TestLib.Util


spec :: TopSpec
spec = describe "VectorMyers" $ do
  describe "Single deletes" $ do
    checkDiff "a" "" [mkDelete (0, 0) (0, 1)]
    checkDiff "ab" "a" [mkDelete (0, 1) (0, 2)]
    checkDiff "ab" "b" [mkDelete (0, 0) (0, 1)]
    checkDiff "\na" "\n" [mkDelete (1, 0) (1, 1)]
    checkDiff "\n\na" "\n\n" [mkDelete (2, 0) (2, 1)]

  describe "Single inserts" $ do
    checkDiff "" "a" [mkInsert (0, 0) (0, 0) "a"]
    checkDiff "\n" "a\n" [mkInsert (0, 0) (0, 0) "a"]
    checkDiff "\n" "\na" [mkInsert (1, 0) (1, 0) "a"]

  describe "Double deletes" $ do
    checkDiff "ab" "" [mkDelete (0, 0) (0, 2)]

    checkDiff "xab" "x" [mkDelete (0, 1) (0, 2), mkDelete (0, 1) (0, 2)]
    checkDiffConsolidated "xab" "x" [mkDelete (0, 1) (0, 3)]

    checkDiff "abc" "b" [mkDelete (0, 0) (0, 1), mkDelete (0, 1) (0, 2)]

  describe "Double inserts" $ do
    checkDiff "" "ab" [mkInsert (0, 0) (0, 0) "ab"]
    checkDiff "x" "xab" [mkInsert (0, 1) (0, 1) "a", mkInsert (0, 2) (0, 2) "b"]
    checkDiffConsolidated "x" "xab" [mkInsert (0, 1) (0, 1) "ab"]

  describe "Longer inserts" $ do
    checkDiff "" "abcde" [mkInsert (0, 0) (0, 0) "abcde"]

  describe "consolidateEditScript" $ do
    it "consolidates two inserts" $ do
      consolidateEditScript [EditInsert 1 1 1, EditInsert 1 2 2] `shouldBe` [EditInsert 1 1 2]
    it "consolidates two deletes" $ do
      consolidateEditScript [EditDelete 1 1, EditDelete 2 2] `shouldBe` [EditDelete 1 2]

  describe "QuickCheck" $ introduceQuickCheck $ modifyMaxSuccess (const 10000) $ do
    describe "Arbitrary text" $ do
      prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
      prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to

    describe "Arbitrary document (series of arbitrary texts with plenty of newlines)" $ do
      prop "Single change" $ \(DocInsertOrDelete (from, to)) -> verifyDiff from to
      prop "Multiple changes" $ \(DocMultiInsertOrDelete (from, to)) -> verifyDiff from to


checkDiff :: (MonadThrow m, MonadIO m) => Text -> Text -> [ChangeEvent] -> SpecFree context m ()
checkDiff from to changes = it (show from <> " -> " <> show to) $ do
  -- Check that the given changes actually work
  applyChangesText changes from `shouldBe` to

  -- Diff produces the desired changse
  diffTextsToChangeEvents from to `shouldBe` changes

checkDiffConsolidated :: (MonadThrow m, MonadIO m) => Text -> Text -> [ChangeEvent] -> SpecFree context m ()
checkDiffConsolidated from to changes = it (show from <> " -> " <> show to <> " (consolidated)") $ do
  -- Check that the given changes actually work
  applyChangesText changes from `shouldBe` to

  -- Diff produces the desired changse
  diffTextsToChangeEventsConsolidate from to `shouldBe` changes

verifyDiff :: Text -> Text -> Bool
verifyDiff from to = (applyChangesText change from == to) && (applyChangesText changeConsolidated from == to)
  where
    change = diffTextsToChangeEvents from to
    changeConsolidated = diffTextsToChangeEventsConsolidate from to

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
