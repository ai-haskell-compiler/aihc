{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.Base.Rank0Spec
  ( genRank0UpTo8Spec
  , genRank0UpTo16Spec
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

genRank0UpTo8Spec :: forall s. (Typeable s, BitRead s, Rank0 s) => s -> Spec
genRank0UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank0 10010010 over [0..8] should be 001223445" $ requireProperty $ do
    let bs = fromJust (bitRead "10010010") :: s
    fmap (rank0 bs) [0..8] === [0, 0, 1, 2, 2, 3, 4, 4, 5]

genRank0UpTo16Spec :: forall s. (Typeable s, BitRead s, Rank0 s) => s -> Spec
genRank0UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank0 11011010 00000000 over [0..16]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: s
    fmap (rank0 bs) [0..16] === [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  it "rank0 11011010 10000000 over [0..16]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 10000000" :: s
    fmap (rank0 bs) [0..16] === [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.InternalSpec" $ do
  genRank0UpTo8Spec (undefined :: Word8)
  genRank0UpTo8Spec (undefined :: Word16)
  genRank0UpTo8Spec (undefined :: Word32)
  genRank0UpTo8Spec (undefined :: Word64)
  genRank0UpTo8Spec (undefined :: [Word8])
  genRank0UpTo16Spec (undefined :: [Word8])
  genRank0UpTo8Spec (undefined :: [Word16])
  genRank0UpTo16Spec (undefined :: [Word16])
  genRank0UpTo8Spec (undefined :: [Word32])
  genRank0UpTo16Spec (undefined :: [Word32])
  genRank0UpTo8Spec (undefined :: [Word64])
  genRank0UpTo16Spec (undefined :: [Word64])
  genRank0UpTo8Spec (undefined :: DV.Vector Word8)
  genRank0UpTo16Spec (undefined :: DV.Vector Word8)
  genRank0UpTo8Spec (undefined :: DV.Vector Word16)
  genRank0UpTo16Spec (undefined :: DV.Vector Word16)
  genRank0UpTo8Spec (undefined :: DV.Vector Word32)
  genRank0UpTo16Spec (undefined :: DV.Vector Word32)
  genRank0UpTo8Spec (undefined :: DV.Vector Word64)
  genRank0UpTo16Spec (undefined :: DV.Vector Word64)
  genRank0UpTo8Spec (undefined :: DVS.Vector Word8)
  genRank0UpTo16Spec (undefined :: DVS.Vector Word8)
  genRank0UpTo8Spec (undefined :: DVS.Vector Word16)
  genRank0UpTo16Spec (undefined :: DVS.Vector Word16)
  genRank0UpTo8Spec (undefined :: DVS.Vector Word32)
  genRank0UpTo16Spec (undefined :: DVS.Vector Word32)
  genRank0UpTo8Spec (undefined :: DVS.Vector Word64)
  genRank0UpTo16Spec (undefined :: DVS.Vector Word64)
  genRank0UpTo8Spec (undefined :: DVU.Vector Bit.Bit)
  genRank0UpTo16Spec (undefined :: DVU.Vector Bit.Bit)
  genRank0UpTo8Spec (undefined :: DVU.Vector BitTS.Bit)
  genRank0UpTo16Spec (undefined :: DVU.Vector BitTS.Bit)
  describe "Different word sizes give the same rank0" $ do
    it "when comparing Word16 and Word64 over bits 0-7" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 8)
      w <- forAll $ G.word8 R.constantBounded
      rank0 w i === rank0 (fromIntegral w :: Word64) i
    it "when comparing Word16 and Word64 over bits 0-15" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 16)
      w <- forAll $ G.word16 R.constantBounded
      rank0 w i === rank0 (fromIntegral w :: Word64) i
    it "when comparing Word32 and Word64 over bits 0-31" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      w <- forAll $ G.word32 R.constantBounded
      rank0 w i === rank0 (fromIntegral w :: Word64) i
    it "when comparing Word32 and Word64 over bits 32-64" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      v <- forAll $ G.word32 R.constantBounded
      w <- forAll $ G.word32 R.constantBounded
      let v64 = fromIntegral v :: Word64
      let w64 = fromIntegral w :: Word64
      rank0 v i + popCount0 w === rank0 ((v64 .<. 32) .|. w64) (i + 32)
    it "when comparing select1 for Word64 form a galois connection" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      w <- forAll $ G.word32 R.constantBounded
      when (1 <= i && i <= popCount0 w) $ do
        rank0 w (select0 w i) === i
        (select0 w (rank0 w (fromIntegral i)) <= fromIntegral i) === True
