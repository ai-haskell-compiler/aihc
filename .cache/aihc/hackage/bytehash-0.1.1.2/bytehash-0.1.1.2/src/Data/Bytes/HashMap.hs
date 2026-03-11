{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Implementation of static hash map data structure.
module Data.Bytes.HashMap
  ( Map
  , empty
  , lookup
  , fromList
  , fromTrustedList
  , fromListWith
  , elements

    -- * Used for testing
  , HashMapException (..)
  , distribution
  , distinctEntropies
  ) where

-- Implementation notes. This module uses a variant of the technique
-- described in http://stevehanov.ca/blog/?id=119 with the big difference
-- being that we do not throw away the keys. You can only throw away the
-- keys in very specific problem domains where you somehow control
-- everything that is going to be looked up.
--
-- General implementation thoughts. It would be really nice to figure
-- out how to parallelize hashing. We currently go one byte at a time.
-- Processing more bytes at a time would cut down on memory loads.
-- However, doing more than one byte at a time is tricky. When you
-- get to the end of a string, you end up having to do some extra
-- finagling to make sure you do not read past the end. I have tried
-- to do this in the past, and it is difficult to do it correctly.
--
-- Other thought: Using a random 64-bit word for each byte is pretty
-- heavy handed. 64-bit words give us 32-bit hashes, but in most cases,
-- we are not building maps that are that big. We really only need
-- 16-bit hashes most of the time (maps with less than 64K values).
-- Switching to 32-bit words would save space. Plus, if we did this,
-- we could also use SSE _mm_add_epi32 and _mm_add_epi32 to process
-- four bytes at a time.

import Prelude hiding (lookup)

import Control.Exception (Exception, throw)
import Control.Monad (when)
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Bytes.HashMap.Internal (Map (Map))
import Data.Bytes.Types (Bytes (Bytes))
import Data.Foldable (foldlM, for_)
import Data.Int (Int32)
import Data.Ord (Down (Down))
import Data.Primitive (ByteArray (..), PrimArray (..))
import Data.Primitive.SmallArray (SmallArray (..))
import Data.Primitive.Unlifted.Array (UnliftedArray, UnliftedArray_ (UnliftedArray))
import Data.Primitive.Unlifted.Array.Primops (UnliftedArray#)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Foreign.Ptr (plusPtr)
import GHC.Exts (ByteArray#, Int (I#), Int#, Ptr, RealWorld, SmallArray#)
import GHC.IO (ioToST)
import GHC.Word (Word (W#), Word32, Word8)
import System.Entropy (CryptHandle, hGetEntropy)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Hash as Hash
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM
import qualified GHC.Exts as Exts

{- | Recover the elements of the hashmap. These are ordered
lexicographically by their corresponding keys. That is, this
function returns the same output regardless of the entropy used
to build the hashmap.
-}
elements :: Map v -> [v]
elements (Map _ _ _ keys vals) = Map.elems (go Map.empty (PM.sizeofSmallArray vals - 1))
 where
  go !acc !ix = case ix of
    (-1) -> acc
    _ ->
      let !k = PM.indexUnliftedArray keys ix
          !v = PM.indexSmallArray vals ix
       in go (Map.insert k v acc) (ix - 1)

{- | Build a static hash map. This may be used on input that comes
from an adversarial user. It always produces a perfect hash map.
-}
fromList :: CryptHandle -> [(Bytes, v)] -> IO (Map v)
fromList h = fromListWith h const

{- | Build a map from keys that are known at compile time.
All keys must be 64 bytes or less. This uses a built-in source
of entropy and is entirely deterministic. An adversarial user
could feed this function keys that cause it to error out rather
than completing.
-}
fromTrustedList :: [(Bytes, v)] -> Map v
fromTrustedList xs = runST $ do
  ref <- newSTRef 0
  fromListWithGen ref askForEntropyST const xs

-- | An empty @Map@.
empty :: Map v
empty = Map mempty mempty mempty mempty mempty

-- | Returns the value associated with the key in the map.
lookup :: Bytes -> Map v -> Maybe v
{-# INLINE lookup #-}
lookup
  (Bytes (ByteArray keyArr) (I# keyOff) (I# keyLen))
  (Map (ByteArray entropyA) (UnliftedArray entropies) (PrimArray offsets) (UnliftedArray keys) (SmallArray vals)) =
    case lookup# (# keyArr, keyOff, keyLen #) (# entropyA, entropies, offsets, keys, vals #) of
      (# (# #) | #) -> Nothing
      (# | v #) -> Just v

-- One compelling optimization done here is that we use sameByteArray
-- to check if the sources of entropy are pointer-wise equal. This is
-- a very inexpensive check, and it ends up being true close to 50%
-- of the time. If it is true, we can avoid hashing a second time.
-- which avoids reading from a place in memory that is essentially
-- random. One way to further improve the performance of this library
-- would be to try to get doubleton buckets to use entropyA by searching
-- for a suitable offset.
lookup# ::
  (# ByteArray#, Int#, Int# #) ->
  (# ByteArray#, UnliftedArray# ByteArray#, ByteArray#, UnliftedArray# ByteArray#, SmallArray# v #) ->
  (# (# #) | v #)
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
          , !(# v #) <- PM.indexSmallArray## vals offsetIx ->
              (# | v #)
          | otherwise -> (# (# #) | #)
        _
          | PM.sizeofByteArray entropyB >= reqEntropy
          , ix <- w2i (unsafeRem (upW32 (Hash.bytes entropyB key)) (i2w sz))
          , offsetIx <- offset + ix
          , bytesEqualsByteArray key (PM.indexUnliftedArray keys offsetIx)
          , !(# v #) <- PM.indexSmallArray## vals offsetIx ->
              (# | v #)
          | otherwise -> (# (# #) | #)
 where
  sz = PM.sizeofUnliftedArray entropies
  reqEntropy = w2i (requiredEntropy (i2w (Bytes.length key)))
  key = Bytes (ByteArray keyArr#) (I# keyOff#) (I# keyLen#)
  entropyA = ByteArray entropyA#
  entropies = UnliftedArray entropies# :: UnliftedArray ByteArray
  keys = UnliftedArray keys# :: UnliftedArray ByteArray
  vals = SmallArray vals#
  offsets = PrimArray offsets# :: PrimArray Int32

unsafeRem :: Word -> Word -> Word
unsafeRem (W# a) (W# b) = W# (Exts.remWord# a b)

fromListWithGen ::
  forall a s v.
  -- | Source of randomness
  a ->
  (a -> Int -> ST s ByteArray) ->
  (v -> v -> v) ->
  [(Bytes, v)] ->
  ST s (Map v)
fromListWithGen h ask combine xs
  | count == 0 = pure (Map mempty mempty mempty mempty mempty)
  | otherwise = do
      let maxLen' =
            w2i $
              requiredEntropy $
                i2w $
                  List.foldl' (\acc (b, _) -> max (Bytes.length b) acc) 0 xs'
          allowedCollisions = ceiling (sqrt (fromIntegral @Int @Double (count + 1))) :: Int
      entropyA <- findInitialEntropy h ask maxLen' count allowedCollisions xs'
      let groups :: [[(Word, (Bytes, v))]]
          groups =
            List.sortOn
              (Down . List.length @[])
              ( List.groupBy
                  (\(x, _) (y, _) -> x == y)
                  ( List.sortOn
                      fst
                      ( List.map
                          (\(b, v) -> (rem (fromIntegral @Word32 @Word (Hash.bytes entropyA b)) (i2w count), (b, v)))
                          xs'
                      )
                  )
              )
      used <- PM.newSmallArray count False
      keys <- PM.newUnliftedArray count (mempty :: ByteArray)
      values <- PM.newSmallArray count (errorThunk @v)
      entropies <- PM.newUnliftedArray count (mempty :: ByteArray)
      offsets <- PM.newPrimArray count
      PM.setPrimArray offsets 0 count (0 :: Int32)
      let {-# SCC goB #-}
          goB :: [ByteArray] -> [[(Word, (Bytes, v))]] -> ST s ()
          goB !_ [] = pure ()
          goB !cache (x : ps) = case x of
            [(w, (key, val))] -> do
              -- Space optimization for singleton buckets. If only one key
              -- hashed to a bucket, we just use entropyA as the entropy
              -- since it is guaranteed to be big enough. Then we use the
              -- offset field to correct the hash. This avoid creating any
              -- additional entropy byte arrays for singleton buckets.
              -- Technically, it should be possible to do this for some
              -- of the doubletons as well. It is just a little more
              -- difficult.
              let ix = w2i (unsafeHeadFst x)
              j <- findUnused used
              PM.writeUnliftedArray entropies ix entropyA
              PM.writePrimArray offsets ix (fromIntegral @Int @Int32 (j - fromIntegral w))
              PM.writeSmallArray used j True
              PM.writeUnliftedArray keys j (Bytes.toByteArray key)
              PM.writeSmallArray values j val
              goB cache ps
            _ -> do
              let ix = w2i (unsafeHeadFst x)
                  keyVals = map snd x
                  !maxGroupLen = List.foldl' (\acc (b, _) -> max (Bytes.length b) acc) 0 keyVals
                  reqEntrSz = w2i (requiredEntropy (i2w maxGroupLen))
              -- As a space optimization, we try out all options from the cache.
              -- If we can reuse random bytes that were used for a different key,
              -- we can save a lot of space. Reuse is frequently possible.
              e <- runExceptT $ for_ (entropyA : cache) $ \entropy ->
                if PM.sizeofByteArray entropy >= reqEntrSz
                  then
                    ExceptT $
                      attempt entropy ix keyVals >>= \case
                        True -> pure (Left ())
                        False -> pure (Right ())
                  else ExceptT (pure (Right ()))
              case e of
                Left () -> goB cache ps
                Right () -> do
                  goD cache 100000 ix keyVals reqEntrSz ps
          updateSlots :: ByteArray -> Int -> [(Bytes, v)] -> ST s ()
          updateSlots !entropy !ix keyVals = do
            PM.writeUnliftedArray entropies ix entropy
            for_ keyVals $ \(key, val) -> do
              let j = fromIntegral @Word @Int (rem (fromIntegral @Word32 @Word (Hash.bytes entropy key)) (i2w count))
              PM.writeSmallArray used j True
              PM.writeUnliftedArray keys j (Bytes.toByteArray key)
              PM.writeSmallArray values j val
          attempt :: ByteArray -> Int -> [(Bytes, v)] -> ST s Bool
          attempt !entropy !ix keyVals = do
            tmpUsed <- PM.cloneSmallMutableArray used 0 count
            allGood <-
              foldlM
                ( \good (key, _) ->
                    if good
                      then do
                        let j = fromIntegral @Word @Int (rem (upW32 (Hash.bytes entropy key)) (i2w count))
                        PM.readSmallArray tmpUsed j >>= \case
                          True -> pure False
                          False -> do
                            PM.writeSmallArray tmpUsed j True
                            pure True
                      else pure False
                )
                True
                keyVals
            if allGood
              then do
                updateSlots entropy ix keyVals
                pure True
              else pure False
          {-# SCC goD #-}
          goD :: [ByteArray] -> Int -> Int -> [(Bytes, v)] -> Int -> [[(Word, (Bytes, v))]] -> ST s ()
          goD !cache !counter !ix keyVals !entropySz zs = do
            entropy <- ask h entropySz
            attempt entropy ix keyVals >>= \case
              True -> goB (entropy : cache) zs
              False -> case counter of
                0 ->
                  throw $
                    HashMapException
                      count
                      (map fst keyVals)
                      ((fmap . fmap . fmap) fst groups)
                _ -> goD cache (counter - 1) ix keyVals entropySz zs
      -- Notice that we do not start out with entropyA. We manually cons that
      -- onto the top every time, so that if it can get reused, it does. We
      -- would rather it get reused than anything else since there is an
      -- optimization in the lookup function that avoids computing the hash
      -- twice if this entropy gets used.
      goB [] groups
      vals' <- PM.unsafeFreezeSmallArray values
      keys' <- PM.unsafeFreezeUnliftedArray keys
      entropies' <- PM.unsafeFreezeUnliftedArray entropies
      offsets' <- PM.unsafeFreezePrimArray offsets
      pure (Map entropyA entropies' offsets' keys' vals')
 where
  -- Combine duplicates upfront.
  xs' :: [(Bytes, v)]
  xs' =
    map
      ( \rs ->
          ( unsafeHeadFst rs
          , List.foldl1' combine (map snd rs)
          )
      )
      (List.groupBy (\(x, _) (y, _) -> x == y) (List.sortOn fst xs))
  count = List.length @[] xs' :: Int

findUnused :: PM.SmallMutableArray s Bool -> ST s Int
findUnused xs = PM.getSizeofSmallMutableArray xs >>= go 0
 where
  go !ix !len =
    if ix < len
      then do
        PM.readSmallArray xs ix >>= \case
          True -> go (ix + 1) len
          False -> pure ix
      else error "findUnused: could not find unused slot"

fromListWith ::
  forall v.
  -- | Source of randomness
  CryptHandle ->
  (v -> v -> v) ->
  [(Bytes, v)] ->
  IO (Map v)
fromListWith h combine xs =
  stToIO
    (fromListWithGen h askForEntropy combine xs)

findInitialEntropy ::
  forall s a v.
  a ->
  (a -> Int -> ST s ByteArray) ->
  Int ->
  Int ->
  Int ->
  [(Bytes, v)] ->
  ST s ByteArray
{-# SCC findInitialEntropy #-}
findInitialEntropy !h ask !maxLen' !count !allowedCollisions xs = go 40
 where
  go :: Int -> ST s ByteArray
  go !ix = do
    entropy <- ask h maxLen'
    let maxCollisions =
          List.foldl'
            (\acc zs -> max acc (List.length @[] zs))
            0
            ( List.group
                ( List.sort
                    (map (\(b, _) -> rem (fromIntegral @Word32 @Word (Hash.bytes entropy b)) (i2w count)) xs)
                )
            )
    if maxCollisions <= allowedCollisions
      then pure entropy
      else case ix of
        0 -> throw (HashMapException (-1) [] [])
        _ -> go (ix - 1)

askForEntropyST :: STRef s Int -> Int -> ST s ByteArray
askForEntropyST !ref !n = do
  counter <- readSTRef ref
  writeSTRef ref $! mod (counter + 1) 8192
  let (_, r) = divMod n 8
  if
    | r /= 0 -> error "bytehash: askForEntropyST, request does not divide 8"
    | n > 8192 -> error "bytehash: askForEntropyST, requested more than 8192"
    | otherwise -> do
        dst <- PM.newPrimArray n
        PM.copyPtrToMutablePrimArray
          dst
          0
          (plusPtr Hash.entropy counter :: Ptr Word8)
          n
        PM.PrimArray x <- PM.unsafeFreezePrimArray dst
        pure (ByteArray x)

askForEntropy :: CryptHandle -> Int -> ST RealWorld ByteArray
askForEntropy !h !n = ioToST $ do
  entropy <- hGetEntropy h n
  when
    (ByteString.length entropy /= n)
    (fail "bytehash: askForEntropy failed, blame entropy")
  dst <- PM.newByteArray n
  ByteString.unsafeUseAsCStringLen entropy $ \(ptr, len) -> do
    let !(PM.MutableByteArray primDst) = dst
    PM.copyPtrToMutablePrimArray (PM.MutablePrimArray primDst) 0 ptr len
  PM.unsafeFreezeByteArray dst

requiredEntropy :: Word -> Word
requiredEntropy n = 8 * n + 8

errorThunk :: a
errorThunk = error "Data.Bytes.HashMap: mistake"

unsafeHeadFst :: [(a, b)] -> a
unsafeHeadFst ((x, _) : _) = x
unsafeHeadFst [] = error "Data.Bytes.HashMap: bad use of unsafeHeadFst"

w2i :: Word -> Int
w2i = fromIntegral

i2w :: Int -> Word
i2w = fromIntegral

upW32 :: Word32 -> Word
upW32 = fromIntegral

bytesEqualsByteArray :: Bytes -> ByteArray -> Bool
bytesEqualsByteArray (Bytes arr1 off1 len1) arr2
  | len1 /= PM.sizeofByteArray arr2 = False
  | otherwise = compareByteArrays arr1 off1 arr2 0 len1 == EQ

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#) =
  compare (I# (Exts.compareByteArrays# ba1# off1# ba2# off2# n#)) 0

data HashMapException = HashMapException !Int [Bytes] [[(Word, Bytes)]]
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

{- | For each slot, gives the number of keys that hash to it
after the first hash function has been applied.
-}
distribution :: Map v -> [(Int, Int)]
distribution (Map entropy entropies _ keys _) =
  let counts = runST $ do
        let sz = PM.sizeofUnliftedArray entropies
        dst <- PM.newPrimArray sz
        PM.setPrimArray dst 0 sz (0 :: Int)
        let go !ix = case ix of
              (-1) -> pure ()
              _ -> do
                let key = PM.indexUnliftedArray keys ix
                let ixA = w2i (unsafeRem (upW32 (Hash.byteArray entropy key)) (i2w sz))
                old <- PM.readPrimArray dst ixA
                PM.writePrimArray dst ixA (old + 1)
                go (ix - 1)
        go (sz - 1)
        PM.unsafeFreezePrimArray dst
   in List.sort $
        List.map
          ( \xs -> case xs of
              [] -> errorWithoutStackTrace "bytehash: distribution impl error"
              y : _ -> (y, List.length xs)
          )
          (List.group (List.sort (Exts.toList counts)))

-- | The number of non-matching entropies being used.
distinctEntropies :: Map v -> Int
distinctEntropies (Map entropy entropies _ _ _) =
  List.length (List.group (List.sort (entropy : Exts.toList entropies)))

sameByteArray :: ByteArray -> ByteArray -> Int#
sameByteArray (ByteArray x) (ByteArray y) =
  Exts.sameMutableByteArray# (Exts.unsafeCoerce# x) (Exts.unsafeCoerce# y)
