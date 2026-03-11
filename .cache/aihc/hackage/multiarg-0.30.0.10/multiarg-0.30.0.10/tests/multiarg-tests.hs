module Main where

import qualified Multiarg.Maddash.Tests
import qualified Grover.Tests
import qualified Telly.Tests
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "all properties"
  [ Multiarg.Maddash.Tests.tests
  , Grover.Tests.tests
  , Telly.Tests.tests
  ]
