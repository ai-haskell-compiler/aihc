{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Test.Hspec
import           Data.Map.Syntax.Tests


------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  describe "simple insertions" insTests
  describe "monoid laws" monoidLaws
  describe "nested block insertions" nestingTests
