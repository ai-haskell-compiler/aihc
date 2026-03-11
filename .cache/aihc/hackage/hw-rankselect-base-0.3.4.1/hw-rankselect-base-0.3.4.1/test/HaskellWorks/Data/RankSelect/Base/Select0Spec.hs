{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.Base.Select0Spec
  ( genSelect0UpTo8Spec
  , genSelect0UpTo16Spec
  , genSelect0UpTo32Spec
  , spec
  ) where

import Control.Monad
import Data.Maybe
import Data.Typeable
import Data.Word
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Select0
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

genSelect0UpTo8Spec :: forall s. (Typeable s, BitRead s, Select0 s) => s -> Spec
genSelect0UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 10010010 over [0..5] should be 023568" $ requireProperty $ do
    let bs = fromJust $ bitRead "10010010" :: Word8
    fmap (select0 bs) [0..5] === [0, 2, 3, 5, 6, 8]

genSelect0UpTo16Spec :: forall s. (Typeable s, BitRead s, Select0 s) => s -> Spec
genSelect0UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 11011010 00 over [0..5]" $ requireProperty $ do
    let bs = fromJust $ bitRead "1101101 000" :: [Bool]
    fmap (select0 bs) [0..5] === [0, 3, 6, 8, 9, 10]
  it "select0 11011010 00000000 over [0..5]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: Word32
    fmap (select0 bs) [0..11] === [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]

genSelect0UpTo32Spec :: forall s. (Typeable s, BitRead s, Select0 s) => s -> Spec
genSelect0UpTo32Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ requireProperty $ do
    let bs = fromJust $ bitRead "11000001 10000000 01000000" :: s
    fmap (select0 bs) [0..19] === [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.InternalSpec" $ do
  genSelect0UpTo8Spec  (undefined :: Word8)
  genSelect0UpTo8Spec  (undefined :: Word16)
  genSelect0UpTo8Spec  (undefined :: Word32)
  genSelect0UpTo8Spec  (undefined :: Word64)
  genSelect0UpTo16Spec (undefined :: Word16)
  genSelect0UpTo16Spec (undefined :: Word32)
  genSelect0UpTo16Spec (undefined :: Word64)
  genSelect0UpTo32Spec (undefined :: Word32)
  genSelect0UpTo32Spec (undefined :: Word64)
  genSelect0UpTo8Spec  (undefined :: [Bool])
  genSelect0UpTo16Spec (undefined :: [Bool])
  genSelect0UpTo32Spec (undefined :: [Bool])
  genSelect0UpTo8Spec  (undefined :: [Word8])
  genSelect0UpTo16Spec (undefined :: [Word8])
  genSelect0UpTo32Spec (undefined :: [Word8])
  genSelect0UpTo8Spec  (undefined :: [Word16])
  genSelect0UpTo16Spec (undefined :: [Word16])
  genSelect0UpTo32Spec (undefined :: [Word16])
  genSelect0UpTo8Spec  (undefined :: [Word32])
  genSelect0UpTo16Spec (undefined :: [Word32])
  genSelect0UpTo32Spec (undefined :: [Word32])
  genSelect0UpTo8Spec  (undefined :: [Word64])
  genSelect0UpTo16Spec (undefined :: [Word64])
  genSelect0UpTo32Spec (undefined :: [Word64])
  genSelect0UpTo8Spec  (undefined :: DV.Vector Word8)
  genSelect0UpTo16Spec (undefined :: DV.Vector Word8)
  genSelect0UpTo32Spec (undefined :: DV.Vector Word8)
  genSelect0UpTo8Spec  (undefined :: DV.Vector Word16)
  genSelect0UpTo16Spec (undefined :: DV.Vector Word16)
  genSelect0UpTo32Spec (undefined :: DV.Vector Word16)
  genSelect0UpTo8Spec  (undefined :: DV.Vector Word32)
  genSelect0UpTo16Spec (undefined :: DV.Vector Word32)
  genSelect0UpTo32Spec (undefined :: DV.Vector Word32)
  genSelect0UpTo8Spec  (undefined :: DV.Vector Word64)
  genSelect0UpTo16Spec (undefined :: DV.Vector Word64)
  genSelect0UpTo32Spec (undefined :: DV.Vector Word64)
  genSelect0UpTo8Spec  (undefined :: DVS.Vector Word8)
  genSelect0UpTo16Spec (undefined :: DVS.Vector Word8)
  genSelect0UpTo32Spec (undefined :: DVS.Vector Word8)
  genSelect0UpTo8Spec  (undefined :: DVS.Vector Word16)
  genSelect0UpTo16Spec (undefined :: DVS.Vector Word16)
  genSelect0UpTo32Spec (undefined :: DVS.Vector Word16)
  genSelect0UpTo8Spec  (undefined :: DVS.Vector Word32)
  genSelect0UpTo16Spec (undefined :: DVS.Vector Word32)
  genSelect0UpTo32Spec (undefined :: DVS.Vector Word32)
  genSelect0UpTo8Spec  (undefined :: DVS.Vector Word64)
  genSelect0UpTo16Spec (undefined :: DVS.Vector Word64)
  genSelect0UpTo32Spec (undefined :: DVS.Vector Word64)
  genSelect0UpTo8Spec  (undefined :: DVU.Vector Bit.Bit)
  genSelect0UpTo16Spec (undefined :: DVU.Vector Bit.Bit)
  genSelect0UpTo32Spec (undefined :: DVU.Vector Bit.Bit)
  genSelect0UpTo8Spec  (undefined :: DVU.Vector BitTS.Bit)
  genSelect0UpTo16Spec (undefined :: DVU.Vector BitTS.Bit)
  genSelect0UpTo32Spec (undefined :: DVU.Vector BitTS.Bit)
  describe "For Word8-Word64" $ do
    it "rank0 for Word16 and Word64 should give same answer for bits 0-7" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 8)
      w <- forAll $ G.word8 R.constantBounded
      rank0 w i === rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word16 and Word64 should give same answer for bits 0-15" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 16)
      w <- forAll $ G.word16 R.constantBounded
      rank0 w i === rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word32 and Word64 should give same answer for bits 0-31" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      w <- forAll $ G.word32 R.constantBounded
      rank0 w i === rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word32 and Word64 should give same answer for bits 32-64" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      v <- forAll $ G.word32 R.constantBounded
      w <- forAll $ G.word32 R.constantBounded
      let v64 = fromIntegral v :: Word64
      let w64 = fromIntegral w :: Word64
      rank0 v i + popCount0 w === rank0 ((v64 .<. 32) .|. w64) (i + 32)
    it "rank0 and select0 for Word64 form a galois connection" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      w <- forAll $ G.word32 R.constantBounded
      when (1 <= i && i <= popCount0 w) $ do
        rank0 w (select0 w i) === i
        (select0 w (rank0 w (fromIntegral i)) <= fromIntegral i) === True
