-- Copyright : 2011-2014 Magnus Therning
-- License   : BSD3
module Main where

import Test.Tasty

import qualified Ini as I
import qualified ReaderI as RI

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests" [I.allTests, RI.allTests]
