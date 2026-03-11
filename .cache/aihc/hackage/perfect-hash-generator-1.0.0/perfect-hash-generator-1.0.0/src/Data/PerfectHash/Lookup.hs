{-# OPTIONS_HADDOCK prune #-}

-- | Note that what is referred to as a \"nonce\" in this library may be
-- better known as a \"<https://en.wikipedia.org/wiki/Salt_(cryptography) salt>\".
module Data.PerfectHash.Lookup (
    LookupTable (LookupTable, nonces, values)
  , size
  , encodeDirectEntry
  , lookup
  ) where

import           Data.Vector      (Vector, (!))
import qualified Data.Vector      as Vector
import           Prelude                  hiding (lookup)

import Data.PerfectHash.Types.Nonces (Nonce (Nonce))
import qualified Data.PerfectHash.Hashing as Hashing
import qualified Data.PerfectHash.Types.Nonces as Nonces


-- | Inputs for the lookup function.
--
-- There are two arrays used in successive stages of the lookup.
-- In this implementation, both arrays are the same length.
data LookupTable a = LookupTable {
    nonces :: Vector Int
    -- ^ This is the intermediate lookup table.
    --
    -- In the lookup process, the key's hash is computed first with a nonce of
    -- zero to obtain an index into this array.
    --
    -- If the value at this index is negative, it is (after negating and
    -- subtracting one) a direct index into the 'values' array.
    -- Otherwise, the value shall be used as a nonce in a second application of
    -- the hashing function to compute the index into the 'values' array.
    --
    -- See the documentation of 'lookup' for details.
  , values :: Vector a
    -- ^ An array of values of arbitrary type.
    --
    -- The objective of the perfect hash is to efficiently retrieve an index into
    -- this array, given the key associated with the value at that index.
  }


size :: LookupTable a -> Hashing.ArraySize
size = Hashing.ArraySize . Vector.length . values


-- NOTE: We subtract one to ensure it's negative even if the
-- zeroeth slot was used. This lets us test for "direct encoding"
-- by checking of the value is negative.
encodeDirectEntry :: Hashing.SlotIndex -> Int
encodeDirectEntry (Hashing.SlotIndex val) =
  subtract 1 $ negate val


-- | NOTE: negation, followed by subtracting 1 is its own self-inverse.
--
-- Example:
-- > a = 7
-- > f(a) = -7 - 1
-- >      = -8
-- >
-- > a' = -8
-- > f(a') = -(-8) - 1
-- >      = 8 - 1
-- >      = 7
decodeDirectEntry :: Int -> Hashing.SlotIndex
decodeDirectEntry val =
  Hashing.SlotIndex $ encodeDirectEntry $ Hashing.SlotIndex val


-- | For embedded applications, this function would usually be re-implemented
-- in C code.
--
-- == Procedure description
-- The lookup procedure is three steps:
--
--     1. Compute the 'Hashing.hash' (with a nonce of zero) of the "key", modulo
--        the length of the 'values' array.
--     2. Use the resulting value as an index into the 'nonces' array.  The value
--        found there represents either a direct index into the 'values' array
--        or a nonce for a second round of hashing.
--
--         * If negative, it is the former.  Negate it (to obtain a positive
--           value) and subtract one to obtain the actual index.
--         * Otherwise, re-compute the hash of the key, using this
--           value instead of zero as the nonce. Again, compute the modulus with
--           respect to the length of the 'values' array.
--
--     3. Use the result of (2) as the index into the 'values' array.
lookup
  :: (Hashing.ToHashableChunks a)
  => LookupTable b
  -> a -- ^ key
  -> b -- ^ value
lookup lookup_table key =

  values lookup_table ! v_key

  where
    table_size = size lookup_table

    Hashing.SlotIndex nonce_index = Hashing.hashToSlot (Nonce 0) table_size key
    nonce = nonces lookup_table ! nonce_index

    -- Negative nonce value indicates that we don't need extra lookup layer
    Hashing.SlotIndex v_key = if Nonces.isDirectSlot nonce
      then decodeDirectEntry nonce
      else Hashing.hashToSlot (Nonces.Nonce nonce) table_size key
