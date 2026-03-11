{-# LANGUAGE FlexibleInstances, BangPatterns #-}
{-|
Module      : Data.Digest.Murmur64
Copyright   : (c) Thomas Schilling 2010
License     : BSD-style

Maintainer  : nominolo@gmail.com
Stability   : experimental
Portability : portable

Type class and primitives for constructing 64 bit hashes using the
MurmurHash2 algorithm.  See <http://murmurhash.googlepages.com> for
details on MurmurHash2.
-}
module Data.Digest.Murmur64
  ( Hash64, asWord64,
    Hashable64(..),
    hash64AddWord64, hash64AddInt, hash64, hash64WithSeed, combine,
  )
where

import Data.Word ( Word64 )
import Numeric ( showHex )
import Data.Bits ( Bits(xor, shiftR) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char ( ord )
import Data.Foldable ( Foldable(foldl') )
import Data.List ( unfoldr )

-- | A 64 bit hash.
newtype Hash64 = Hash64 Word64
  deriving (Eq, Ord, Bounded)

instance Show Hash64 where
  showsPrec _ (Hash64 w) = showString "Hash64 0x" . showHex w

-- | Extract 64 bit word from hash.
asWord64 :: Hash64 -> Word64
asWord64 (Hash64 w) = w

class Hashable64 a where
  hash64Add :: a -> Hash64 -> Hash64

murmur_m :: Word64
murmur_m = 0xc6a4a7935bd1e995

murmur_r :: Int
murmur_r = 47

-- | Add a 64 bit word to the hash.
hash64AddWord64 :: Word64 -> Hash64 -> Hash64
hash64AddWord64 k (Hash64 h) =
  let k1 = k * murmur_m
      k2 = k1 `xor` (k1 `shiftR` murmur_r)
      k3 = k2 * murmur_m
      h1 = h * murmur_m
      h2 = h1 `xor` k3
  in Hash64 h2

hash64AddInt :: Int -> Hash64 -> Hash64
hash64AddInt !k0 = hash64AddWord64 (fromIntegral k0)

hash64AddFoldable :: (Hashable64 a, Foldable c) => c a -> Hash64 -> Hash64
hash64AddFoldable c !h0 = foldl' f h0 c
  where f h a = hash64Add a h

-- | Create a hash using a custom seed.
--
-- The seed should be non-zero, but other than that can be an
-- arbitrary number.  Different seeds will give different hashes, and
-- thus (most likely) different hash collisions.
hash64WithSeed :: Hashable64 a => Word64 -> a -> Hash64
hash64WithSeed seed a = hash64End (hash64Add a (Hash64 seed))

-- | Create a hash using the default seed.
hash64 :: Hashable64 a => a -> Hash64
hash64 = hash64WithSeed defaultSeed

-- | Combine two hash generators.  E.g.,
--
-- @
--   hashFoo (Foo a) = hash64AddInt 1 `combine` hash64Add a
-- @
combine :: (Hash64 -> Hash64) -> (Hash64 -> Hash64) -> (Hash64 -> Hash64)
combine x y = y . x

hash64End :: Hash64 -> Hash64
hash64End (Hash64 h) =
  let h1 = h `xor` (h `shiftR` murmur_r)
      h2 = h1 * murmur_m
      h3 = h2 `xor` (h2 `shiftR` murmur_r)
  in Hash64 h3

defaultSeed :: Word64
defaultSeed = 0xdeadbeef -- not 0, otherwise hash64 [0] == hash64 []

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

instance Hashable64 Char where
  hash64Add c = hash64AddInt (ord c)

instance Hashable64 Int where
  hash64Add = hash64AddInt

instance Hashable64 Word64 where
  hash64Add = hash64AddWord64

instance Hashable64 a => Hashable64 [a] where
  hash64Add = hash64AddFoldable

instance Hashable64 Integer where
  -- Within Int range, make sure they hash to exactly the same value
  hash64Add i0
   | i0 >= fromIntegral (minBound :: Int) &&
      i0 <= fromIntegral (maxBound :: Int)
   = hash64AddInt (fromIntegral i0)
   | otherwise
   -- Prefix by sign, then hash the raw data words, starting with LSB
   = hash64Add (signum i0 > 0) `combine`
     hash64AddFoldable (unfoldr f (abs i0) :: [Word64])
    where
      f i | i == 0 = Nothing
      f i =
        let (i', a) = quotRem i maxWord in
        Just (fromIntegral a, i')
      maxWord = fromIntegral (maxBound :: Word64) + 1 :: Integer

instance Hashable64 Bool where
  hash64Add False = hash64AddWord64 1
  hash64Add True = hash64AddWord64 2

instance Hashable64 a => Hashable64 (Maybe a) where
  hash64Add Nothing = hash64AddWord64 3
  hash64Add (Just a) = hash64AddWord64 4 `combine` hash64Add a

instance (Hashable64 a, Hashable64 b) => Hashable64 (Either a b) where
  hash64Add (Left a) = hash64AddWord64 5 `combine` hash64Add a
  hash64Add (Right b) = hash64AddWord64 6 `combine` hash64Add b

instance Hashable64 () where
  hash64Add () = hash64AddWord64 7

instance (Hashable64 a, Hashable64 b) => Hashable64 (a, b) where
  hash64Add (a, b) = hash64Add a `combine` hash64Add b

instance (Hashable64 a, Hashable64 b, Hashable64 c)
    => Hashable64 (a, b, c) where
  hash64Add (a, b, c) =
    hash64Add a `combine` hash64Add b `combine` hash64Add c

instance (Hashable64 a, Hashable64 b, Hashable64 c, Hashable64 d)
    => Hashable64 (a, b, c, d) where
  hash64Add (a, b, c, d) =
    hash64Add a `combine` hash64Add b `combine`
    hash64Add c `combine` hash64Add d

instance Hashable64 B.ByteString where
  hash64Add bs h = B.foldl' go (hash64AddWord64 8 h) bs
    where go acc b = hash64AddWord64 (fromIntegral b) acc

instance Hashable64 L.ByteString where
  hash64Add bs h = L.foldl' go (hash64AddWord64 9 h) bs
    where go acc b = hash64AddWord64 (fromIntegral b) acc
