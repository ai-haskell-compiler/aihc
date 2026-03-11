{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Safe                 #-}


-- | Implements the specialized hash function for
-- this perfect hashing algorithm.
--
-- C code that makes use of the perfect hash table output must exactly
-- re-implement this 'hash' function.
module Data.PerfectHash.Hashing where

import           Data.Binary          (encode)
import           Data.Bits            (xor, (.&.))
import qualified Data.ByteString.Lazy as B (unpack)
import           Data.Char            (ord)
import           Data.Foldable        (foldl')
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.PerfectHash.Types.Nonces as Nonces
import Data.PerfectHash.Types.Nonces (Nonce)

-- Types

newtype SlotIndex = SlotIndex {getIndex :: Int}

newtype Hash = Hash {getHash :: Int}
  deriving (Eq, Show)

newtype ArraySize = ArraySize Int
  deriving Show


-- * Constants

-- | This choice of prime number @0x01000193@ was taken from the Python implementation
-- on <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's page>.
primeFNV :: Int
primeFNV = 0x01000193


mask32bits :: Int
mask32bits = 0xffffffff


-- * Class instances

-- | Mechanism for a key to be decomposed into units processable by the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV-1a> hashing algorithm.
class ToHashableChunks a where
  toHashableChunks :: a -> [Hash]

instance ToHashableChunks Int where
  toHashableChunks = map (Hash. fromIntegral) . B.unpack . encode

instance ToHashableChunks String where
  toHashableChunks = map $ Hash . ord

instance ToHashableChunks Text where
  toHashableChunks = toHashableChunks . T.unpack

-- Utilities

generateArrayIndices :: ArraySize -> [SlotIndex]
generateArrayIndices (ArraySize size) = map SlotIndex [0..(size - 1)]


-- * Main functions

hashToSlot :: ToHashableChunks a =>
     Nonce
  -> ArraySize
  -> a -- ^ key
  -> SlotIndex
hashToSlot nonce (ArraySize size) key =
  SlotIndex $ getHash (hash nonce key) `mod` size


-- Used in the 'hash' function
getNonzeroNonceVal :: Nonce -> Int
getNonzeroNonceVal (Nonces.Nonce nonce) =
  if nonce == 0
    then primeFNV
    else nonce


-- | The interface is comparable to the
-- <https://hackage.haskell.org/package/hashable-1.2.6.1/docs/Data-Hashable.html#v:hashWithSalt hashWithSalt>
-- function from the @hashable@ package.
--
-- Uses the \"FNV-1a\" algorithm from the
-- <http://isthe.com/chongo/tech/comp/fnv/#FNV-1a FNV website>:
--
-- > hash = offset_basis
-- > for each octet_of_data to be hashed
-- >         hash = hash xor octet_of_data
-- >         hash = hash * FNV_prime
-- > return hash
hash :: ToHashableChunks a =>
     Nonce -- ^ nonce
  -> a -- ^ key
  -> Hash
hash nonce =

  -- NOTE: This must be 'foldl', not 'foldr'
  Hash . foldl' combine d . toHashableChunks
  where
    d = getNonzeroNonceVal nonce

    combine acc = (.&. mask32bits) . (* primeFNV) . xor acc . getHash
