module Data.BitSetWord8Spec where

import           Data.Char             (chr)
import           Data.Word             (Word8)

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Data.BitSetWord8

spec :: Spec
spec = do
    describe "fromList" $ do
        prop "creates empty BitSetWord8 from empty list" $ forAll (choose (0, 0xff :: Word8)) $ \n ->
            member (fromList []) n `shouldBe` False

        prop "creates fullfilled BitSetWord8 from fullfilled list" $ forAll (choose (0, 0xff :: Word8)) $ \n ->
            member (fromList [chr 0 .. chr 0xff]) n `shouldBe` True

    describe "rfc5234Digit'" $ do
        prop "recognizes any digit" $ forAll (choose (0x30, 0x39 :: Word8)) $ \n ->
            member (fromList rfc5234Digit') n `shouldBe` True

        prop "unrecognizes any non-digit characters (lower)" $ forAll (choose (0, 0x2f :: Word8)) $ \n ->
            member (fromList rfc5234Digit') n `shouldBe` False

        prop "unrecognizes any non-digit characters (higher)" $ forAll (choose (0x3a, 0xff :: Word8)) $ \n ->
            member (fromList rfc5234Digit') n `shouldBe` False

    describe "rfc5234Alpha'" $ do
        prop "recognizes any upper case alphabet" $ forAll (choose (0x41, 0x5a :: Word8)) $ \n ->
            member (fromList rfc5234Alpha') n `shouldBe` True

        prop "recognizes any lower case alphabet" $ forAll (choose (0x61, 0x7a :: Word8)) $ \n ->
            member (fromList rfc5234Alpha') n `shouldBe` True

        prop "unrecognizes any non-alphabet (lower)" $ forAll (choose (0, 0x40 :: Word8)) $ \n ->
            member (fromList rfc5234Alpha') n `shouldBe` False

        prop "unrecognizes any non-alphabet (middle)" $ forAll (choose (0x5b, 0x60 :: Word8)) $ \n ->
            member (fromList rfc5234Alpha') n `shouldBe` False

        prop "unrecognizes any non-alphabet (higher)" $ forAll (choose (0x7b, 0xff :: Word8)) $ \n ->
            member (fromList rfc5234Alpha') n `shouldBe` False
