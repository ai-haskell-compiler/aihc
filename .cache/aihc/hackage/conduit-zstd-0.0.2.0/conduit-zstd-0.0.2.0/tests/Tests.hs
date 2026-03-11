{- Copyright 2018 Luis Pedro Coelho
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.ByteString as B
import Conduit
import Data.Conduit.List (sourceList)

import Data.Conduit.Zstd

tests :: TestTree
tests =
  testProperty "roundtrip" $ \input -> ioProperty $ do
    output' <- runConduit (sourceList input .| compress 1 .| decompress .| sinkList)
    pure $ B.concat input === B.concat output'

main :: IO ()
main = defaultMain tests
