module Main where

import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import Test.Hspec

import Control.Concurrent.BFQueueSpec as BFQueueSpec
import Control.Concurrent.STM.TBFQueueSpec as TBFQueueSpec


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec $ do
    describe "BFQueue" $ do
      BFQueueSpec.spec
    describe "TBFQueue" $ do
      TBFQueueSpec.spec
