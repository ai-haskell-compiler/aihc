{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Main where

import KeyedVals.Handle.Mem (new)
import System.IO (
  BufferMode (..),
  hSetBuffering,
  stderr,
  stdout,
 )
import Test.Hspec (hspec)
import Test.KeyedVals.Hspec (
  Spec,
  afterAll,
  beforeAll,
  checkHandle,
  closeFixture,
  describe,
  setupFixture,
 )


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hspec spec


spec :: Spec
spec =
  describe "Using the Mem handle" $
    beforeAll (new >>= setupFixture) $
      afterAll closeFixture checkHandle
