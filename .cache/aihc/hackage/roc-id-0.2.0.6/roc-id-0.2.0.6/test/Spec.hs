{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ROC.ID
import ROC.ID.Digit
import ROC.ID.Gender
import ROC.ID.Location
import ROC.ID.Serial

import Data.Char
    ( intToDigit )
import Test.Hspec
    ( describe, hspec, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonEmptyList (..)
    , applyArbitrary3
    , arbitraryBoundedEnum
    , genericShrink
    , property
    )

import qualified Data.Text as T
import qualified Data.Vector.Sized as V

instance Arbitrary Digit where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

instance Arbitrary Gender where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

instance Arbitrary Identity where
  arbitrary = applyArbitrary3 Identity
  shrink = genericShrink

instance Arbitrary Location where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

instance Arbitrary Serial where
  arbitrary = Serial . V.fromTuple <$> arbitrary
  shrink (Serial v) = Serial <$> traverse shrink v

main :: IO ()
main = hspec $

  describe "parseIdentity" $ do

    it "successfully parses valid identification numbers" $
      property $ \(i :: Identity) ->
        parseIdentity (T.pack $ show i) `shouldBe` Right i

    it "does not parse identification numbers that are too short" $
      property $ \(i :: Identity) n -> do
        let newLength = n `mod` 10
        let invalidIdentity = T.pack $ take newLength $ show i
        parseIdentity invalidIdentity `shouldBe` Left InvalidLength

    it "does not parse identification numbers that are too long" $
      property $ \(i :: Identity) (NonEmpty s) -> do
        let invalidIdentity = T.pack $ show i <> s
        parseIdentity invalidIdentity `shouldBe` Left InvalidLength

    it "does not parse identification numbers with invalid gender codes" $
      property $ \(i :: Identity) (c :: Int) -> do
        let invalidGenderCode = intToDigit $ ((c `mod` 8) + 3) `mod` 10
        let invalidIdentity = T.pack $
              take 1 (show i) <> [invalidGenderCode] <> drop 2 (show i)
        parseIdentity invalidIdentity `shouldBe` Left InvalidGender

    it "does not parse identification numbers with invalid location codes" $
      property $ \(i :: Identity) (c :: Int) -> do
        let invalidLocationCode = intToDigit $ c `mod` 10
        let invalidIdentity = T.pack $ invalidLocationCode : drop 1 (show i)
        parseIdentity invalidIdentity `shouldBe` Left InvalidLocation

    it "does not parse identification numbers with invalid checksums" $
      property $ \(i :: Identity) (c :: Int) -> do
        let invalidChecksum = intToDigit $
              ((c `mod` 9) + fromEnum (identityChecksum i) + 1) `mod` 10
        let invalidIdentity = T.pack $ take 9 (show i) <> [invalidChecksum]
        parseIdentity invalidIdentity `shouldBe` Left InvalidChecksum

