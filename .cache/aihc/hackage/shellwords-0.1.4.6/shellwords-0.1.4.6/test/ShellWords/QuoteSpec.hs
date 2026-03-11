{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Some test cases ported from Rust:
--
-- <https://github.com/tmiasko/shell-words/blob/045e4dccd2478ccc8bfa91bd0fe449dfe5473496/src/lib.rs#L475-L488>
module ShellWords.QuoteSpec
  ( spec
  ) where

import Prelude hiding (truncate)

import ShellWords.Quote (join, quote)
import Test.Hspec

spec :: Spec
spec = do
  describe "quote" $ do
    it "single-quotes the empty string" $ do
      quote "" `shouldBe` "''"

    it "escapes a single quote" $ do
      quote "'" `shouldBe` "''\\'''"

    it "leaves un-special characters alone" $ do
      quote "abc" `shouldBe` "abc"

    it "quotes newlines" $ do
      quote "a \n  b" `shouldBe` "'a \n  b'"

    it "quotes newlines and escapes single-quotes" $ do
      quote "X'\nY" `shouldBe` "'X'\\''\nY'"

    it "quotes tildes correctly" $ do
      quote "~root" `shouldBe` "'~root'"

  describe "join" $ do
    it "joins arguments" $ do
      join ["a", "b", "c"] `shouldBe` "a b c"

    it "quotes arguments" $ do
      join [" ", "$", "\n"] `shouldBe` "' ' '$' '\n'"
