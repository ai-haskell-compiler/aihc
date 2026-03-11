{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.Base.Rank1Spec
  ( genRank1UpTo8Spec
  , genRank1UpTo16Spec
  , spec
  ) where

import Control.Monad
import Data.Maybe
import Data.Typeable
import Data.Word
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
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

genRank1UpTo8Spec :: forall s. (Typeable s, BitRead s, Rank1 s) => s -> Spec
genRank1UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank1 10010010 over [0..8] should be 011122233" $ requireProperty $ do
    let bs = fromJust (bitRead "10010010") :: s
    fmap (rank1 bs) [0..8] === [0, 1, 1, 1, 2, 2, 2, 3, 3]

genRank1UpTo16Spec :: forall s. (Typeable s, BitRead s, Rank1 s) => s -> Spec
genRank1UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank1 11011010 00000000 over [0..9]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: s
    fmap (rank1 bs) [0..16] === [0, 1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
  it "rank1 11011010 10000000 over [0..9]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 10000000" :: s
    fmap (rank1 bs) [0..16] === [0, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6]

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.InternalSpec" $ do
  genRank1UpTo8Spec (undefined :: Word8)
  genRank1UpTo8Spec (undefined :: Word16)
  genRank1UpTo8Spec (undefined :: Word32)
  genRank1UpTo8Spec (undefined :: Word64)
  genRank1UpTo8Spec (undefined :: DV.Vector Word8)
  genRank1UpTo16Spec (undefined :: DV.Vector Word8)
  genRank1UpTo8Spec (undefined :: DV.Vector Word16)
  genRank1UpTo16Spec (undefined :: DV.Vector Word16)
  genRank1UpTo8Spec (undefined :: DV.Vector Word32)
  genRank1UpTo16Spec (undefined :: DV.Vector Word32)
  genRank1UpTo8Spec (undefined :: DV.Vector Word64)
  genRank1UpTo16Spec (undefined :: DV.Vector Word64)
  genRank1UpTo8Spec (undefined :: DVS.Vector Word8)
  genRank1UpTo16Spec (undefined :: DVS.Vector Word8)
  genRank1UpTo8Spec (undefined :: DVS.Vector Word16)
  genRank1UpTo16Spec (undefined :: DVS.Vector Word16)
  genRank1UpTo8Spec (undefined :: DVS.Vector Word32)
  genRank1UpTo16Spec (undefined :: DVS.Vector Word32)
  genRank1UpTo8Spec (undefined :: DVS.Vector Word64)
  genRank1UpTo16Spec (undefined :: DVS.Vector Word64)
  genRank1UpTo16Spec (undefined :: DVU.Vector Bit.Bit)
  genRank1UpTo16Spec (undefined :: DVU.Vector BitTS.Bit)
  describe "For Word8-Word64" $ do
    it "rank1 for Word16 and Word64 should give same answer for bits 0-7" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 8)
      w <- forAll $ G.word8 R.constantBounded
      rank1 w i === rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word16 and Word64 should give same answer for bits 0-15" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 16)
      w <- forAll $ G.word16 R.constantBounded
      rank1 w i === rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word32 and Word64 should give same answer for bits 0-31" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      w <- forAll $ G.word32 R.constantBounded
      rank1 w i === rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word32 and Word64 should give same answer for bits 32-64" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      v <- forAll $ G.word32 R.constantBounded
      w <- forAll $ G.word32 R.constantBounded
      let v64 = fromIntegral v :: Word64
      let w64 = fromIntegral w :: Word64
      rank1 v i + popCount1 w === rank1 ((v64 .<. 32) .|. w64) (i + 32)
    it "rank1 and select1 for Word64 form a galois connection" $ requireProperty $ do
      i <- forAll $ G.word64 (R.linear 0 32)
      w <- forAll $ G.word32 R.constantBounded
      when (1 <= i && i <= popCount1 w) $ do
        rank1 w (select1 w i) === i
        (select1 w (rank1 w (fromIntegral i)) <= fromIntegral i) === True
