module Main where

import Spec qualified
import Test.Hspec
import Test.Hspec.Runner (defaultConfig, hspecWith)
import Prelude

main :: IO ()
main =
  hspecWith defaultConfig $
    parallel Spec.spec
