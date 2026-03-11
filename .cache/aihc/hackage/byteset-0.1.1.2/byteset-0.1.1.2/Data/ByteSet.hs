
{-# LANGUAGE BangPatterns, DeriveGeneric #-}

-- | Inspired in the @Data.IntSet@ API, a similar API where the elements
--   of the set are bytes (values of type 'Word8').
module Data.ByteSet (
    -- * Types
    ByteSet
  , Word8
    -- * Query
  , null
  , size
  , member
  , notMember
    -- * Construction
  , empty
  , singleton
  , insert
  , delete
    -- * Combine
  , union
  , unions
  , difference
  , intersection
    -- * Filter
  , filter
    -- * Map
  , map
    -- * Folds
  , foldr
    -- * List conversion
  , elems
  , toList
  , fromList
  ) where

import Prelude
  ( Eq (..), Ord (..)
  , (+), (-), ($), (*)
  , fromIntegral
  , Show (..)
    )
import Data.Word (Word8, Word64)
import Data.Int (Int)
import Data.Bits
import Data.Bool
import Control.Category
import Control.Applicative (Applicative (..))
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Functor
import GHC.Generics (Generic)
import Data.Binary (Binary (..))
import Data.Binary.Put (putWord64le)
import Data.Binary.Get (getWord64le)

-- | Set of bytes ('Word8'). Note that NF and WHNF are equivalent
--   for values of type 'ByteSet'.
data ByteSet = ByteSet
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Generic)

generalGet :: (Word64 -> Int -> a) -> Word8 -> ByteSet -> a
{-# INLINE generalGet #-}
generalGet f w (ByteSet s1 s2 s3 s4)
  | w <  64   = f s1  i
  | w < 128   = f s2 (i -  64)
  | w < 192   = f s3 (i - 128)
  | otherwise = f s4 (i - 192)
  where
    i = fromIntegral w

generalSet :: (Word64 -> Int -> Word64) -> Word8 -> ByteSet -> ByteSet
{-# INLINE generalSet #-}
generalSet f w (ByteSet s1 s2 s3 s4)
  | w <  64   = ByteSet          (f s1   i      ) s2 s3 s4
  | w < 128   = ByteSet s1       (f s2 $ i -  64) s3 s4
  | w < 192   = ByteSet s1 s2    (f s3 $ i - 128) s4
  | otherwise = ByteSet s1 s2 s3 (f s4 $ i - 192)
  where
    i = fromIntegral w

generalOp :: (Word64  -> Word64  -> Word64 )
          ->  ByteSet -> ByteSet -> ByteSet
{-# INLINE generalOp #-}
generalOp f (ByteSet s1 s2 s3 s4)
            (ByteSet t1 t2 t3 t4) =
  ByteSet (f s1 t1) (f s2 t2)
          (f s3 t3) (f s4 t4)

generalFun :: (Int -> Word64 -> Word64) -> ByteSet -> ByteSet -- tons of fun!
{-# INLINE generalFun #-}
generalFun f (ByteSet s1 s2 s3 s4) =
  ByteSet (f 0 s1) (f 1 s2) (f 2 s3) (f 3 s4)

----------------------------------------------------------------------
----------------------------------------------------------------------
-- API

-- | /O(1)/. Cardinality of the byteset.
size :: ByteSet -> Int
size (ByteSet s1 s2 s3 s4) =
    popCount s1 + popCount s2
  + popCount s3 + popCount s4

-- | /O(1)/. Is the byteset empty?
null :: ByteSet -> Bool
null = (==0) . size

-- | /O(1)/. Is the value a member of the byteset?
member :: Word8 -> ByteSet -> Bool
member = generalGet testBit

-- | /O(1)/. Is the element not in the set?
notMember :: Word8 -> ByteSet -> Bool
notMember w = not . member w

-- | /O(1)/. The empty byteset.
empty :: ByteSet
empty = ByteSet 0 0 0 0

-- | /O(1)/. Add a value to the byteset.
insert :: Word8 -> ByteSet -> ByteSet
insert = generalSet setBit

-- | /O(1)/. A byteset of one element.
singleton :: Word8 -> ByteSet
singleton w = insert w empty

-- | /O(1)/. Delete a byte in the byteset. Returns the original byteset when the byte was not present.
delete :: Word8 -> ByteSet -> ByteSet
delete = generalSet clearBit

-- | /O(1)/. The union of two bytesets.
union :: ByteSet -> ByteSet -> ByteSet
union = generalOp (.|.)

-- | The union of a list of bytesets. Just a fold over the list using 'union'.
unions :: [ByteSet] -> ByteSet
unions = F.foldl' union empty

-- | /O(1)/. Difference between two bytesets.
difference :: ByteSet -> ByteSet -> ByteSet
difference = generalOp $ \w1 w2 -> w1 .&. complement w2

-- | /O(1)/. The intersection of two bytesets.
intersection :: ByteSet -> ByteSet -> ByteSet
intersection = generalOp (.&.)

-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: (Word8 -> Bool) -> ByteSet -> ByteSet
filter f = generalFun $ \i w ->
  let b0 = i * 64
      go acc (-1) = acc
      go !acc n = if testBit w n && f (fromIntegral $ b0 + n)
                     then go (setBit acc n) $ n - 1
                     else go         acc    $ n - 1
  in  go 0 63

bits :: Word64 -> [Int]
bits w = L.filter (testBit w) [0..63]

-- | /O(n)/. Fold the elements in the byteset using the given right-associative binary operator.
foldr :: (Word8 -> a -> a) -> a -> ByteSet -> a
foldr f r0 (ByteSet s1 s2 s3 s4) =
  let g  = f . fromIntegral
      r1 = F.foldr g r0 $ fmap (+192) $ bits s4
      r2 = F.foldr g r1 $ fmap (+128) $ bits s3
      r3 = F.foldr g r2 $ fmap (+ 64) $ bits s2
  in       F.foldr g r3 $               bits s1

-- | /O(n)/. Map a function over a byteset.
map :: (Word8 -> Word8) -> ByteSet -> ByteSet
map f = foldr (insert . f) empty

-- | /O(n)/. The elements of a byteset in ascending order.
elems :: ByteSet -> [Word8]
elems = foldr (:) []

-- | /O(n)/. An alias of 'elems'.
toList :: ByteSet -> [Word8]
{-# INLINE toList #-}
toList = elems

-- | /O(n)/. Create a byteset from a list of bytes.
fromList :: [Word8] -> ByteSet
fromList = F.foldr insert empty

instance Show ByteSet where
  show = show . elems

instance Binary ByteSet where
  put (ByteSet s1 s2 s3 s4) =
       putWord64le s1 *> putWord64le s2
    *> putWord64le s3 *> putWord64le s4
  get = ByteSet <$> getWord64le <*> getWord64le
                <*> getWord64le <*> getWord64le
