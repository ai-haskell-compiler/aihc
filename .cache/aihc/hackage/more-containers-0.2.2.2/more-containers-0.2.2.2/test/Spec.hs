module Main where

import Test.Hspec (hspec)

import Spec.Multimap
import Spec.Multiset

main :: IO ()
main = hspec $ do
  multisetSpec
  listMultimapSpec
  seqMultimapSpec
  setMultimapSpec
