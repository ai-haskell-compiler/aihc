{-# LANGUAGE FlexibleInstances, BangPatterns #-}
{-|
Module      : Data.Digest.Murmur32
Copyright   : (c) Thomas Schilling 2010
License     : BSD-style

Maintainer  : nominolo@gmail.com
Stability   : experimental
Portability : portable

Type class and primitives for constructing 32 bit hashes using the
MurmurHash2 algorithm.  See <http://murmurhash.googlepages.com> for
details on MurmurHash2.
-}
module Data.Digest.Murmur32
  ( Hash32, asWord32,
    Hashable32(..),
    hash32AddWord32, hash32AddInt, hash32, hash32WithSeed
  )
where

import Data.Word ( Word32 )
import Numeric ( showHex )
import Data.Bits ( Bits(xor, shiftR), FiniteBits(finiteBitSize) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char ( ord )
import Data.Foldable ( Foldable(foldl') )
import Data.List ( unfoldr )

-- | A 32 bit hash.
newtype Hash32 = Hash32 Word32
  deriving (Eq, Ord, Bounded)

instance Show Hash32 where
  showsPrec _ (Hash32 w) = showString "Hash32 0x" . showHex w

-- | Extract 32 bit word from hash.
asWord32 :: Hash32 -> Word32
asWord32 (Hash32 w) = w

class Hashable32 a where
  hash32Add :: a -> Hash32 -> Hash32

murmur_m :: Word32
murmur_m = 0x5bd1e995

murmur_r :: Int
murmur_r = 24

hash32AddWord32 :: Word32 -> Hash32 -> Hash32
hash32AddWord32 k (Hash32 h) =
  let k1 = k * murmur_m
      k2 = k1 `xor` (k1 `shiftR` murmur_r)
      k3 = k2 * murmur_m
      h1 = h * murmur_m
      h2 = h1 `xor` k3
  in Hash32 h2

hash32AddInt :: Int -> Hash32 -> Hash32
hash32AddInt !k0
  | finiteBitSize (undefined :: Int) <= 32     -- Int is 32 bits
    = hash32AddWord32 (fromIntegral k0)
  | otherwise                            -- Int is 64 bits
    = hash32AddWord32 (fromIntegral k0) `combine`
      hash32AddWord32 (fromIntegral (k0 `shiftR` 32))

hash32AddFoldable :: (Hashable32 a, Foldable c) => c a -> Hash32 -> Hash32
hash32AddFoldable c !h0 = foldl' f h0 c
  where f h a = hash32Add a h

-- | Create a hash using a custom seed.
--
-- The seed should be non-zero, but other than that can be an
-- arbitrary number.  Different seeds will give different hashes, and
-- thus (most likely) different hash collisions.
hash32WithSeed :: Hashable32 a => Word32 -> a -> Hash32
hash32WithSeed seed a = hash32End (hash32Add a (Hash32 seed))

-- | Create a hash using the default seed.
hash32 :: Hashable32 a => a -> Hash32
hash32 = hash32WithSeed defaultSeed

-- | Combine two hash generators.  E.g.,
--
-- @
--   hashFoo (Foo a) = hash32AddInt 1 `combine` hash32Add a
-- @
combine :: (Hash32 -> Hash32) -> (Hash32 -> Hash32) -> (Hash32 -> Hash32)
combine x y = y . x

hash32End :: Hash32 -> Hash32
hash32End (Hash32 h) =
  let h1 = h `xor` (h `shiftR` 13)
      h2 = h1 * murmur_m
      h3 = h2 `xor` (h2 `shiftR` 15)
  in Hash32 h3

defaultSeed :: Word32
defaultSeed = 0xdeadbeef -- not 0, otherwise hash32 [0] == hash32 []

{-

On a CPU with two multipliers and two ALUs, Murmur2 can process one
word in every two cycles + set up (2 cycles) and finish (3 cycles).

Here's the data flow graph:

@
     h    k1   k2  k3  ...
     |    |     |   |
     |    * m   |   |
     |    |\    |   |
     |    | >> r    |
     |    | /   |   |
     |   xor    *   |
     |    |     |\  |
     * m  * m   | >> r
      \  /      |/  |
       xor     xor  * m
         \     /    |\
          * m * m   | >> r
           \ /      |/
           xor     xor
             \     /
              * m * m
               \ /
               xor
                 \
                  ...
@
-}

-- -------------------------------------------------------------------
-- Instances

instance Hashable32 Char where
  hash32Add c = hash32AddInt (ord c)

instance Hashable32 Int where
  hash32Add = hash32AddInt

instance Hashable32 Word32 where
  hash32Add = hash32AddWord32

instance Hashable32 a => Hashable32 [a] where
  hash32Add = hash32AddFoldable

instance Hashable32 Integer where
  -- Within Int range, make sure they hash to exactly the same value
  hash32Add i0
   | i0 >= fromIntegral (minBound :: Int) &&
      i0 <= fromIntegral (maxBound :: Int)
   = hash32AddInt (fromIntegral i0)
   | otherwise
   -- Prefix by sign, then hash the raw data words, starting with LSB
   = hash32Add (signum i0 > 0) `combine`
     hash32AddFoldable (unfoldr f (abs i0) :: [Word32])
    where
      f i | i == 0 = Nothing
      f i =
        let (i', a) = quotRem i maxWord in
        Just (fromIntegral a, i')
      maxWord = fromIntegral (maxBound :: Word32) + 1 :: Integer

instance Hashable32 Bool where
  hash32Add False = hash32AddWord32 1
  hash32Add True = hash32AddWord32 2

instance Hashable32 a => Hashable32 (Maybe a) where
  hash32Add Nothing = hash32AddWord32 3
  hash32Add (Just a) = hash32AddWord32 4 `combine` hash32Add a

instance (Hashable32 a, Hashable32 b) => Hashable32 (Either a b) where
  hash32Add (Left a) = hash32AddWord32 5 `combine` hash32Add a
  hash32Add (Right b) = hash32AddWord32 6 `combine` hash32Add b

instance Hashable32 () where
  hash32Add () = hash32AddWord32 7

instance (Hashable32 a, Hashable32 b) => Hashable32 (a, b) where
  hash32Add (a, b) = hash32Add a `combine` hash32Add b

instance (Hashable32 a, Hashable32 b, Hashable32 c)
    => Hashable32 (a, b, c) where
  hash32Add (a, b, c) =
    hash32Add a `combine` hash32Add b `combine` hash32Add c

instance (Hashable32 a, Hashable32 b, Hashable32 c, Hashable32 d)
    => Hashable32 (a, b, c, d) where
  hash32Add (a, b, c, d) =
    hash32Add a `combine` hash32Add b `combine`
    hash32Add c `combine` hash32Add d

instance Hashable32 B.ByteString where
  hash32Add bs h = B.foldl' go (hash32AddWord32 8 h) bs
    where go acc b = hash32AddWord32 (fromIntegral b) acc

instance Hashable32 L.ByteString where
  hash32Add bs h = L.foldl' go (hash32AddWord32 9 h) bs
    where go acc b = hash32AddWord32 (fromIntegral b) acc
