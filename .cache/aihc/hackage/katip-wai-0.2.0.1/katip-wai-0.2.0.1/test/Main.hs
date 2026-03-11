module Main (main) where

import qualified Spec
import Test.Hspec (hspec, parallel)


main :: IO ()
main = hspec $ parallel Spec.spec
