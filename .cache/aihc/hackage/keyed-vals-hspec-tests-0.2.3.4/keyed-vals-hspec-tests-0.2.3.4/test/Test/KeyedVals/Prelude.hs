{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Test.KeyedVals.CheckHandle
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Test.KeyedVals.Prelude (
  -- * functions
  orThrowHandleErr,
  throwHandleErr,
  withGlobOf,

  -- * module re-eports
  module Control.Monad,
  module Control.Exception,
  module Data.ByteString,
  module Data.List.NonEmpty,
  module Numeric.Natural,
  module KeyedVals.Handle,
  module Test.Hspec,
  module Test.Hspec.Benri,
  module Test.KeyedVals.Types,
) where

import Control.Exception (throwIO)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import KeyedVals.Handle (Glob, Handle, HandleErr, Key, close, mkGlob)
import Numeric.Natural (Natural)
import Test.Hspec
import Test.Hspec.Benri
import Test.KeyedVals.Types


withGlobOf :: ByteString -> (Glob -> IO a) -> IO a
withGlobOf x action = do
  case mkGlob x of
    Nothing -> throwIO $ userError "bad test pattern"
    Just g -> action g


throwHandleErr :: HandleErr -> IO ()
throwHandleErr = throwIO . userError . show


orThrowHandleErr :: IO (Either HandleErr ()) -> IO ()
orThrowHandleErr action = action >>= either throwHandleErr pure
