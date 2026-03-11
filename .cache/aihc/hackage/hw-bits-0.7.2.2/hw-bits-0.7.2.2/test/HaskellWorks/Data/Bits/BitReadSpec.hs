{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitReadSpec (spec) where

import Data.Word
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Bit            as Bit
import qualified Data.Vector         as DV
import qualified Data.Vector.Unboxed as DVU

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.BitReadSpec" $ do
  it "bitRead \"10000000 101\" :: Maybe [Word8]" . requireTest $ do
    let w = bitRead "10000000 101" :: Maybe [Bool]
    w === Just [True, False, False, False, False, False, False, False, True, False, True]
  it "bitRead \"10000000 101\" :: Maybe [Word8]" $ requireTest $ do
    let w = bitRead "10000000 101" :: Maybe [Word8]
    w === Just [1, 5]
  it "bitRead \"11100100 10101111 1\" :: Maybe [Word8]" . requireTest $ do
    let ws = bitRead "11100100 10101111 1" :: Maybe [Word8]
    ws === Just [39, 245, 1]
  it "bitRead \"\" :: Maybe [Word8]" . requireTest $ do
    let ws = bitRead "" :: Maybe [Word8]
    ws === Just []

  it "bitRead \"10000000 101\" :: Maybe (DV.Vector Word8)" . requireTest $ do
    let v = bitRead "10000000 101" :: Maybe (DV.Vector Word8)
    v === Just [1, 5]
  it "bitRead \"11100100 10101111 1\" :: Maybe (DV.Vector Word8)" . requireTest $ do
    let v = bitRead "11100100 10101111 1" :: Maybe (DV.Vector Word8)
    v === Just [39, 245, 1]
  it "bitRead \"11100100 10101111 1\" :: Maybe (DV.Vector Word16)" . requireTest $ do
    let v = bitRead "11100100 10101111 1" :: Maybe (DV.Vector Word16)
    v === Just [39 + 62720, 1]
  it "bitRead \"\" :: Maybe (DV.Vector Word8)" . requireTest $ do
    let v = bitRead "" :: Maybe (DV.Vector Word8)
    v === Just []

  it "bitRead \"10000000 101\" :: Maybe (DVU.Vector Bit.Bit)" . requireTest $ do
    let v = bitRead "10000000 101" :: Maybe (DVU.Vector Bit.Bit)
    v === Just [1,0,0,0,0,0,0,0, 1,0,1]
  it "bitRead \"11100100 10101111 1\" :: Maybe (DVU.Vector Bit.Bit)" . requireTest $ do
    let v = bitRead "11100100 10101111 1" :: Maybe (DVU.Vector Bit.Bit)
    v === Just [1,1,1,0,0,1,0,0, 1,0,1,0,1,1,1,1, 1]
  it "bitRead \"11100100 10101111 1\" :: Maybe (DVU.Vector Bit.Bit)" . requireTest $ do
    let v = bitRead "11100100 10101111 1" :: Maybe (DVU.Vector Bit.Bit)
    v === Just [1,1,1,0,0,1,0,0, 1,0,1,0,1,1,1,1, 1]
  it "bitRead \"\" :: Maybe (DVU.Vector Bit.Bit)" . requireTest $ do
    let v = bitRead "" :: Maybe (DVU.Vector Bit.Bit)
    v === Just []

  it "bitShow (8 :: Word8)" . requireTest $ do
    let bs = bitShow (8 :: Word8)
    bs === "00010000"
  it "bitShow (8 :: Word64)" . requireTest $ do
    let bs = bitShow (8 :: Word64)
    bs === "00010000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "bitShow [0x0102040810204080 :: Word64]" . requireTest $ do
    let bs = bitShow ([0x0102040810204080] :: [Word64])
    bs === "00000001 00000010 00000100 00001000 00010000 00100000 01000000 10000000"
