{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.FromByteStringSpec (spec) where

import Data.Word
import HaskellWorks.Data.FromByteString
import Test.Hspec

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

{- HLINT ignore "Redundant do" -}

spec :: Spec
spec = describe "HaskellWorks.Data.FromByteStringSpec" $ do
  describe "For DVS.Vector Word8" $ do
    it "fromByteString (BS.unpack []) :: DVS.Vector Word8" $
      let v = fromByteString (BS.pack []) :: DVS.Vector Word8 in
      v `shouldBe` DVS.fromList []
    it "fromByteString (BS.unpack [0x1]) :: DVS.Vector Word8" $
      let v = fromByteString (BS.pack [0x1]) :: DVS.Vector Word8 in
      v `shouldBe` DVS.fromList [0x1]
    it "fromByteString (BS.unpack [0x1, 0x2]) :: DVS.Vector Word8" $
      let v = fromByteString (BS.pack [0x1, 0x2]) :: DVS.Vector Word8 in
      v `shouldBe` DVS.fromList [0x1, 0x02]
    it "fromByteString (BS.unpack [0x1, 0x2, 0x04]) :: DVS.Vector Word8" $
      let v = fromByteString (BS.pack [0x1, 0x2, 0x04]) :: DVS.Vector Word8 in
      v `shouldBe` DVS.fromList [0x01, 0x2, 0x04]
  describe "For DVS.Vector Word16" $ do
    it "fromByteString (BS.unpack []) :: DVS.Vector Word16" $
      let v = fromByteString (BS.pack []) :: DVS.Vector Word16 in
      v `shouldBe` DVS.fromList []
    it "fromByteString (BS.unpack [0x1]) :: DVS.Vector Word16" $
      let v = fromByteString (BS.pack [0x1]) :: DVS.Vector Word16 in
      v `shouldBe` DVS.fromList [0x1]
    it "fromByteString (BS.unpack [0x1, 0x2]) :: DVS.Vector Word16" $
      let v = fromByteString (BS.pack [0x1, 0x2]) :: DVS.Vector Word16 in
      v `shouldBe` DVS.fromList [0x0201]
    it "fromByteString (BS.unpack [0x1, 0x2, 0x10]) :: DVS.Vector Word16" $
      let v = fromByteString (BS.pack [0x1, 0x2, 0x10]) :: DVS.Vector Word16 in
      v `shouldBe` DVS.fromList [0x0201, 0x10]
  describe "For DVS.Vector Word32" $ do
    it "fromByteString (BS.unpack []) :: DVS.Vector Word32" $
      let v = fromByteString (BS.pack []) :: DVS.Vector Word32 in
      v `shouldBe` DVS.fromList []
    it "fromByteString (BS.unpack [0x1]) :: DVS.Vector Word32" $
      let v = fromByteString (BS.pack [0x1]) :: DVS.Vector Word32 in
      v `shouldBe` DVS.fromList [0x1]
    it "fromByteString (BS.unpack [0x1, 0x2, 0x4, 0x8]) :: DVS.Vector Word32" $
      let v = fromByteString (BS.pack [0x1, 0x2, 0x4, 0x8]) :: DVS.Vector Word32 in
      v `shouldBe` DVS.fromList [0x08040201]
    it "fromByteString (BS.unpack [0x1, 0x2, 0x4, 0x8, 0x10]) :: DVS.Vector Word32" $
      let v = fromByteString (BS.pack [0x1, 0x2, 0x4, 0x8, 0x10]) :: DVS.Vector Word32 in
      v `shouldBe` DVS.fromList [0x08040201, 0x10]
  describe "For DVS.Vector Word64" $ do
    it "fromByteString (BS.unpack []) :: DVS.Vector Word64" $
      let v = fromByteString (BS.pack []) :: DVS.Vector Word64 in
      v `shouldBe` DVS.fromList []
    it "fromByteString (BS.unpack [0x1]) :: DVS.Vector Word64" $
      let v = fromByteString (BS.pack [0x1]) :: DVS.Vector Word64 in
      v `shouldBe` DVS.fromList [0x1]
    it "fromByteString (BS.unpack [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80]) :: DVS.Vector Word64" $
      let v = fromByteString (BS.pack [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80]) :: DVS.Vector Word64 in
      v `shouldBe` DVS.fromList [0x8040201008040201]
    it "fromByteString (BS.unpack [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 0x81]) :: DVS.Vector Word64" $
      let v = fromByteString (BS.pack [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 0x81]) :: DVS.Vector Word64 in
      v `shouldBe` DVS.fromList [0x8040201008040201, 0x81]
