-- | This is an internal module. You probably don't need to import this.
--
-- = WARNING
--
-- Definitions in this module allow violating invariants that would otherwise be
-- guaranteed by non-internal modules. Use at your own risk!
--
module Regex.Internal.Unique
  ( Unique(..)
  , UniqueSet
  , empty
  , member
  , insert
  ) where

import Data.Bits ((.|.), (.&.), unsafeShiftL, finiteBitSize)
import qualified Data.IntSet as IS

-- | A unique ID. Must be >= 0.
newtype Unique = Unique { unUnique :: Int }

-- | A set of 'Unique's.

-- The bitmask is a set for IDs 0..63 on 64-bit and 0..31 on 32-bit.
-- Set operations on this are very fast and speed up the common case of small
-- regexes a little bit, at the cost of a little more memory.
data UniqueSet = UniqueSet {-# UNPACK #-} !Int !IS.IntSet

empty :: UniqueSet
empty = UniqueSet 0 IS.empty

member :: Unique -> UniqueSet -> Bool
member (Unique u) (UniqueSet m is)
  | u < finiteBitSize (0 :: Int) = m .&. (1 `unsafeShiftL` u) /= 0
  | otherwise = u `IS.member` is

insert :: Unique -> UniqueSet -> UniqueSet
insert (Unique u) (UniqueSet m is)
  | u < finiteBitSize (0 :: Int) = UniqueSet (m .|. (1 `unsafeShiftL` u)) is
  | otherwise = UniqueSet m (IS.insert u is)
