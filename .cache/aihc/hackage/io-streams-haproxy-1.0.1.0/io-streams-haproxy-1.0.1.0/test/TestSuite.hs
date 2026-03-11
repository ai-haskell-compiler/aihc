module Main where

import qualified System.IO.Streams.Network.HAProxy.Tests as HAProxy
import           Test.Framework                          (defaultMain, testGroup)


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.HAProxy" HAProxy.tests ]
