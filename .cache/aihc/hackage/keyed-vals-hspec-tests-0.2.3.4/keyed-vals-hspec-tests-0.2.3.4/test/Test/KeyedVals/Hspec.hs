{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Test.KeyedVals.Hspec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Uses "Test.Hspec" to validate the behaviour of a 'Handle' implementation
-}
module Test.KeyedVals.Hspec (
  -- * a test fixture
  checkHandle,

  -- * setup/teardown hspec tests
  setupFixture,
  closeFixture,

  -- * module re-export
  module Test.KeyedVals.Prelude,
) where

import qualified Test.KeyedVals.CheckHandle as LessTyped
import qualified Test.KeyedVals.CheckTypedHandle as Typed
import Test.KeyedVals.Prelude


checkHandle :: SpecWith (Handle IO)
checkHandle = do
  LessTyped.spec
  Typed.spec


setupFixture :: Handle IO -> IO (Handle IO)
setupFixture h = do
  void $ LessTyped.setupFixture h
  void $ Typed.setupFixture h
  pure h


closeFixture :: Handle IO -> IO ()
closeFixture = close
