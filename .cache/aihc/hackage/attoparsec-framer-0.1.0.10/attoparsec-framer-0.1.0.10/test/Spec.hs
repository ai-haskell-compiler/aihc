{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Attoparsec.ToyFrameSpec as ToyFrame
import System.IO (
  BufferMode (..),
  hSetBuffering,
  stderr,
  stdout,
 )
import Test.Hspec


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  hspec $ do
    ToyFrame.spec
