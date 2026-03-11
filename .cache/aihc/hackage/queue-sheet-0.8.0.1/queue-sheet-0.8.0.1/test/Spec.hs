------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : queue sheet test runner
-- Copyright   : Copyright (c) 2020-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (queue-sheet:test)
import qualified QueueSheet.File.Test

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [ QueueSheet.File.Test.tests
    ]
