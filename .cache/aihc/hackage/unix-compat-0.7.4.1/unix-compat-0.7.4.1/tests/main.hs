module Main where

import MkstempSpec
import LinksSpec
import ProcessSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    mkstempSpec
    linksSpec
    processSpec
