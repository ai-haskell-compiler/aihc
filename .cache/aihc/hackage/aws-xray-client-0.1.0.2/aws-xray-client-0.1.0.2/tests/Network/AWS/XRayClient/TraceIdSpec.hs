{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.AWS.XRayClient.TraceIdSpec
  ( spec
  ) where

import Prelude

import qualified Data.Text as T
import Network.AWS.XRayClient.TraceId
import System.Random
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

instance Arbitrary XRayTraceId where
  arbitrary = do
    -- aws expects a timestamp in a specific range for their hex
    timeStamp <- choose (1000000000, 4000000000)
    gen <- mkStdGen . getPositive <$> arbitrary
    pure . fst $ makeXRayTraceId timeStamp gen

instance Arbitrary XRaySegmentId where
  arbitrary = fst . generateXRaySegmentId . mkStdGen . getPositive <$> arbitrary

instance Arbitrary XRayTraceIdHeaderData where
  arbitrary = genericArbitrary

spec :: Spec
spec = do
  describe "generateXRayTraceId" $ do
    it "should always be of length 35" $ property $ \(XRayTraceId traceId) ->
      T.length traceId `shouldBe` 35

  describe "parseXRayTraceIdHeaderData/makeXRayTraceIdHeaderValue" $ do
    it "should be idempotent between writes/reads" $ property $ \headerData ->
      parseXRayTraceIdHeaderData (makeXRayTraceIdHeaderValue headerData)
        `shouldBe` Just headerData
