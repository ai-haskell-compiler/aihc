module Main where

import Test.Framework (defaultMain)

import qualified Test.System.ZMQ4.Patterns.Clone.Internal
import qualified Test.System.ZMQ4.Patterns.RequestReply

main :: IO ()
main = defaultMain [
    Test.System.ZMQ4.Patterns.Clone.Internal.tests
  , Test.System.ZMQ4.Patterns.RequestReply.tests
  ]
