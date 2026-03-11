module Main where

import Test.Hspec

import qualified Data.CallStackSpec

spec :: Spec
spec = do
  describe "Data.CallStack" Data.CallStackSpec.spec

main :: IO ()
main = hspec spec
