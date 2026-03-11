{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.System.Linux.Proc
  ( tests
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Either (isRight)

import           Hedgehog (Property, discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

import           System.Linux.Proc

import           Test.System.Linux.Proc.Hedgehog

prop_read_MemInfo :: Property
prop_read_MemInfo =
  propertyWithTests 10 $ do
    mi <- H.evalEither =<< liftIO readProcMemInfo
    assertShow mi (memTotal mi > memAvailable mi)
    assertShow mi (memTotal mi > memFree mi)
    assertShow mi (memTotal mi > memBuffers mi)
    assertShow mi (memSwapTotal mi >= memSwapFree mi)

prop_read_ProcessIds :: Property
prop_read_ProcessIds =
  propertyWithTests 10 $ do
    pids <- H.evalEither =<< liftIO getProcProcessIds
    assertShow pids (length pids > 0)

prop_read_process_tcp :: Property
prop_read_process_tcp =
  propertyWithTests 50 $ do
    pids <- H.evalEither =<< liftIO getProcProcessIds
    pid <- H.forAll $ Gen.element pids
    eTcp <- liftIO $ readProcTcpSockets pid
    case eTcp of
      Left (ProcReadError {}) -> H.success -- Ignore pid that do not have a tcp entry.
      Left _ -> H.failure
      Right _tcp -> H.success -- Is there anything we can check here?
    H.cover 80 "  interesting" (isRight eTcp)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
