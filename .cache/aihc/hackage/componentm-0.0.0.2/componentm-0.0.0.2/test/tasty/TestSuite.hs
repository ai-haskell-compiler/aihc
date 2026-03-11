{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO

import           Test.Tasty         (defaultMainWithIngredients, testGroup)
import           Test.Tasty.Runners (consoleTestReporter, listingTests)

import qualified ComponentTest      as Component

main :: IO ()
main = defaultMainWithIngredients
  [listingTests, consoleTestReporter]
  (testGroup "componentm library" [Component.tests])
