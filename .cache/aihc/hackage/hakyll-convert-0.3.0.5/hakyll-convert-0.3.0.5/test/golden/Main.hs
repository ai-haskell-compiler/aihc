module Main where

import qualified Golden.Blogger as Blogger
import qualified Golden.IO as IO
import qualified Golden.Wordpress as Wordpress
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [ Blogger.goldenTests,
      IO.goldenTests,
      Wordpress.goldenTests
    ]
