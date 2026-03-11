{-# LANGUAGE OverloadedStrings #-}
module Main (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.String
import           Data.String.Builder

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "build" $ do
    it "can be used to construct multi-line string literals in a monadic way" $ do
      build $ do
        "foo"
        "bar"
        "baz"
      `shouldBe` "foo\nbar\nbaz\n"

    it "works for arbitrary input" $ do
      property $
        \l -> (build . sequence_ . map fromString) l == unlines l
