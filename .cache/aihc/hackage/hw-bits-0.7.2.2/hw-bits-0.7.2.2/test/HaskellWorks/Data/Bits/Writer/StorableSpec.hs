{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.Writer.StorableSpec where

import Data.Word
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                   as DVS
import qualified HaskellWorks.Data.Bits.Writer.Storable as WS

{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Writer.StorableSpec" $ do
  it "unsafeWriteBit" $ requireTest $ do
    let v :: DVS.Vector Word64 = DVS.create $ do
          mw <- WS.newWriter 10
          WS.unsafeWriteBit mw 1
          WS.unsafeWriteBit mw 0
          WS.unsafeWriteBit mw 1
          WS.unsafeWriteBit mw 1
          WS.unsafeWriteBit mw 0
          WS.unsafeWriteBit mw 0
          WS.unsafeWriteBit mw 1
          WS.unsafeWriteBit mw 1
          WS.unsafeWriteBit mw 1
          WS.unsafeWriteBit mw 0
          WS.unsafeWriteBit mw 0
          WS.unsafeWriteBit mw 0
          WS.written mw

    v === DVS.fromList [0b111001101]
  it "unsafeWriteBits 1" $ requireTest $ do
    let v :: DVS.Vector Word64 = DVS.create $ do
          mw <- WS.newWriter 10
          WS.unsafeWriteBits mw 1 1
          WS.unsafeWriteBits mw 1 0
          WS.unsafeWriteBits mw 1 1
          WS.unsafeWriteBits mw 1 1
          WS.unsafeWriteBits mw 1 0
          WS.unsafeWriteBits mw 1 0
          WS.unsafeWriteBits mw 1 1
          WS.unsafeWriteBits mw 1 1
          WS.unsafeWriteBits mw 1 1
          WS.unsafeWriteBits mw 1 0
          WS.unsafeWriteBits mw 1 0
          WS.unsafeWriteBits mw 1 0
          WS.written mw

    v === DVS.fromList [0b111001101]
  it "unsafeWriteBits 2" $ requireTest $ do
    let v :: DVS.Vector Word64 = DVS.create $ do
          mw <- WS.newWriter 10
          WS.unsafeWriteBits mw 1 0b1
          WS.unsafeWriteBits mw 1 0b0
          WS.unsafeWriteBits mw 2 0b11
          WS.unsafeWriteBits mw 2 0b00
          WS.unsafeWriteBits mw 3 0b111
          WS.unsafeWriteBits mw 3 0b000
          WS.written mw
    v === DVS.fromList [0b111001101]
  it "unsafeWriteBits 2" $ requireTest $ do
    let v :: DVS.Vector Word64 = DVS.create $ do
          mw <- WS.newWriter 10
          WS.unsafeWriteBits mw 1 0b1
          WS.unsafeWriteBits mw 1 0b0
          WS.unsafeWriteBits mw 2 0b11
          WS.unsafeWriteBits mw 2 0b00
          WS.unsafeWriteBits mw 3 0b111
          WS.unsafeWriteBits mw 3 0b000
          WS.unsafeWriteBits mw 4 0b1111
          WS.unsafeWriteBits mw 4 0b0000
          WS.unsafeWriteBits mw 5 0b11111
          WS.unsafeWriteBits mw 5 0b00000
          WS.unsafeWriteBits mw 6 0b111111
          WS.unsafeWriteBits mw 6 0b000000
          WS.unsafeWriteBits mw 7 0b1111111
          WS.unsafeWriteBits mw 7 0b0000000
          WS.unsafeWriteBits mw 8 0b11111111
          WS.unsafeWriteBits mw 8 0b00000000
          WS.unsafeWriteBits mw 9 0b111111111
          WS.unsafeWriteBits mw 9 0b000000000
          WS.written mw
    v === DVS.fromList
      [ 0b1111111100000001111111000000111111000001111100001111000111001101
      , 0b0000000000000000000000000000000000000000000000011111111100000000
      ]
  it "unsafeWriteBits 2" $ requireTest $ do
    let v :: DVS.Vector Word64 = DVS.create $ do
          mw <- WS.newWriter 10
          WS.unsafeWriteBits mw 9 0b111111111
          WS.unsafeWriteBits mw 9 0b000000000
          WS.unsafeWriteBits mw 8 0b11111111
          WS.unsafeWriteBits mw 8 0b00000000
          WS.unsafeWriteBits mw 7 0b1111111
          WS.unsafeWriteBits mw 7 0b0000000
          WS.unsafeWriteBits mw 6 0b111111
          WS.unsafeWriteBits mw 6 0b000000
          WS.unsafeWriteBits mw 5 0b11111
          WS.unsafeWriteBits mw 5 0b00000
          WS.unsafeWriteBits mw 4 0b1111
          WS.unsafeWriteBits mw 4 0b0000
          WS.unsafeWriteBits mw 3 0b111
          WS.unsafeWriteBits mw 3 0b000
          WS.unsafeWriteBits mw 2 0b11
          WS.unsafeWriteBits mw 2 0b00
          WS.unsafeWriteBits mw 1 0b1
          WS.unsafeWriteBits mw 1 0b0
          WS.written mw
    v === DVS.fromList
      [ 0b1111000000111111000000011111110000000011111111000000000111111111
      , 0b0000000000000000000000000000000000000001001100011100001111000001
      ]
