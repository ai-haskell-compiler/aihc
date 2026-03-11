{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Implementation of static hash map data structure.
module Data.Bytes.HashMap.Word
  ( Map
  , lookup
  , fromList
  , fromTrustedList
  , fromListWith

    -- * Used for testing
  , distribution
  , distinctEntropies
  ) where

import Prelude hiding (lookup)

import Data.Bytes.Types (Bytes (Bytes))
import Data.Int (Int32)
import Data.Primitive (ByteArray (..), PrimArray (..))
import Data.Primitive.Unlifted.Array (UnliftedArray, UnliftedArray_ (UnliftedArray))
import Data.Primitive.Unlifted.Array.Primops (UnliftedArray#)
import GHC.Exts (ByteArray#, Int (I#), Int#, Word#)
import GHC.Word (Word (W#), Word32)
import System.Entropy (CryptHandle)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Hash as Hash
import qualified Data.Bytes.HashMap as Lifted
import qualified Data.Bytes.HashMap.Internal as Lifted
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM
import qualified GHC.Exts as Exts

{- | A static perfect hash table where the keys are byte arrays. This
  table cannot be updated after its creation, but all lookups have
  guaranteed O(1) worst-case cost. It consumes linear space. This
  is an excellent candidate for use with compact regions.
-}
data Map
  = Map
      !ByteArray -- top-level entropy
      !(UnliftedArray ByteArray) -- entropies
      !(PrimArray Int32) -- offset to apply to hash, could probably be 32 bits
      !(UnliftedArray ByteArray) -- keys
      !(PrimArray Word) -- values

fromLifted :: Lifted.Map Word -> Map
fromLifted (Lifted.Map a b c d e) = Map a b c d (Exts.fromList (Exts.toList e))

fromList :: CryptHandle -> [(Bytes, Word)] -> IO Map
fromList h = fmap fromLifted . Lifted.fromList h

fromListWith ::
  -- | Source of randomness
  CryptHandle ->
  (Word -> Word -> Word) ->
  [(Bytes, Word)] ->
  IO Map
fromListWith h c xs = fmap fromLifted (Lifted.fromListWith h c xs)

{- | Build a map from keys that are known at compile time.
All keys must be 64 bytes or less. This uses a built-in source
of entropy and is entirely deterministic. An adversarial user
could feed this function keys that cause it to error out rather
than completing.
-}
fromTrustedList :: [(Bytes, Word)] -> Map
fromTrustedList = fromLifted . Lifted.fromTrustedList

lookup :: Bytes -> Map -> Maybe Word
{-# INLINE lookup #-}
lookup
  (Bytes (ByteArray keyArr) (I# keyOff) (I# keyLen))
  (Map (ByteArray entropyA) (UnliftedArray entropies) (PrimArray offsets) (UnliftedArray keys) (PrimArray vals)) =
    case lookup# (# keyArr, keyOff, keyLen #) (# entropyA, entropies, offsets, keys, vals #) of
      (# (# #) | #) -> Nothing
      (# | v #) -> Just (W# v)

lookup# ::
  (# ByteArray#, Int#, Int# #) ->
  (# ByteArray#, UnliftedArray# ByteArray#, ByteArray#, UnliftedArray# ByteArray#, ByteArray# #) ->
  (# (# #) | Word# #)
{-# NOINLINE lookup# #-}
lookup# (# keyArr#, keyOff#, keyLen# #) (# entropyA#, entropies#, offsets#, keys#, vals# #)
  | sz == 0 = (# (# #) | #)
  | PM.sizeofByteArray entropyA < reqEntropy = (# (# #) | #)
  | ixA <- w2i (unsafeRem (upW32 (Hash.bytes entropyA key)) (i2w sz))
  , entropyB <- PM.indexUnliftedArray entropies ixA
  , offset <- fromIntegral @Int32 @Int (PM.indexPrimArray offsets ixA) =
      case sameByteArray entropyA entropyB of
        1#
          | ix <- ixA
          , offsetIx <- offset + ix
          , bytesEqualsByteArray key (PM.indexUnliftedArray keys offsetIx)
          , !(W# v) <- PM.indexPrimArray vals offsetIx ->
              (# | v #)
          | otherwise -> (# (# #) | #)
        _
          | PM.sizeofByteArray entropyB >= reqEntropy
          , ix <- w2i (unsafeRem (upW32 (Hash.bytes entropyB key)) (i2w sz))
          , offsetIx <- offset + ix
          , bytesEqualsByteArray key (PM.indexUnliftedArray keys offsetIx)
          , !(W# v) <- PM.indexPrimArray vals offsetIx ->
              (# | v #)
          | otherwise -> (# (# #) | #)
 where
  sz = PM.sizeofUnliftedArray entropies
  reqEntropy = w2i (requiredEntropy (i2w (Bytes.length key)))
  key = Bytes (ByteArray keyArr#) (I# keyOff#) (I# keyLen#)
  entropyA = ByteArray entropyA#
  entropies = UnliftedArray entropies# :: UnliftedArray ByteArray
  keys = UnliftedArray keys# :: UnliftedArray ByteArray
  vals = PrimArray vals# :: PrimArray Word
  offsets = PrimArray offsets# :: PrimArray Int32

unsafeRem :: Word -> Word -> Word
unsafeRem (W# a) (W# b) = W# (Exts.remWord# a b)

i2w :: Int -> Word
i2w = fromIntegral

requiredEntropy :: Word -> Word
requiredEntropy n = 8 * n + 8

w2i :: Word -> Int
w2i = fromIntegral

bytesEqualsByteArray :: Bytes -> ByteArray -> Bool
bytesEqualsByteArray (Bytes arr1 off1 len1) arr2
  | len1 /= PM.sizeofByteArray arr2 = False
  | otherwise = compareByteArrays arr1 off1 arr2 0 len1 == EQ

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#) =
  compare (I# (Exts.compareByteArrays# ba1# off1# ba2# off2# n#)) 0

upW32 :: Word32 -> Word
upW32 = fromIntegral

distribution :: Map -> [(Int, Int)]
distribution (Map entropy entropies offsets keys vals) =
  Lifted.distribution
    (Lifted.Map entropy entropies offsets keys (Exts.fromList (Exts.toList vals)))

-- | The number of non-matching entropies being used.
distinctEntropies :: Map -> Int
distinctEntropies (Map entropy entropies _ _ _) =
  List.length (List.group (List.sort (entropy : Exts.toList entropies)))

sameByteArray :: ByteArray -> ByteArray -> Int#
sameByteArray (ByteArray x) (ByteArray y) =
  Exts.sameMutableByteArray# (Exts.unsafeCoerce# x) (Exts.unsafeCoerce# y)
