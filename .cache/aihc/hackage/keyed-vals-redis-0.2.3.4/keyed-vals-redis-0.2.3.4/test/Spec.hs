{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Main where

import Control.Exception (onException)
import Data.ByteString.Char8 (unpack)
import Data.Proxy (Proxy (..))
import KeyedVals.Handle.Redis (Handle, new)
import System.Environment (setEnv)
import System.IO (
  BufferMode (..),
  hSetBuffering,
  stderr,
  stdout,
 )
import System.TmpProc.Docker.Redis
import Test.Hspec (hspec, mapSubject)
import Test.Hspec.TmpProc
import Test.KeyedVals.Hspec (
  Spec,
  afterAll,
  beforeAll,
  checkHandle,
  closeFixture,
  setupFixture,
 )


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hspec spec


spec :: Spec
spec =
  tdescribe "Using a redis Handle" $
    beforeAll setupHandles $
      afterAll closeHandles $
        mapSubject fst checkHandle


setupHandles :: IO (Fixture (HList '[ProcHandle TmpRedis]))
setupHandles = do
  procHandles <- startupAll $ testProc `HCons` HNil
  flip onException (terminateAll procHandles) $ do
    let redisUrl = unpack $ hUri $ handleOf @"a-redis-db" Proxy procHandles
        addProcHandles x = (x, procHandles)
    setEnv "REDIS_URL" redisUrl
    h <- new
    addProcHandles <$> setupFixture h


closeHandles :: Fixture (HList '[ProcHandle TmpRedis]) -> IO ()
closeHandles f = do
  closeFixture $ fst f
  terminateAll $ snd f


type Fixture a = (Handle IO, a)


testProc :: TmpRedis
testProc = TmpRedis []
