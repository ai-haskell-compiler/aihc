-- |
-- Utility for construction of vectors when the size is not known ahead.
module VectorExtras.Accumulator
  ( Accumulator,
    init,
    add,
    addList,
    addFoldable,

    -- * Execution
    toVector,
    toReverseVector,
  )
where

import qualified Base.Generic
import Base.Prelude hiding (fromListN, init, length)
import Data.Vector.Generic (Vector, fromListN)

-- |
-- Finalise the accumulator as vector.
{-# INLINE toVector #-}
toVector :: (Vector v a) => Accumulator a -> v a
toVector (Accumulator size list) =
  Base.Generic.fromReverseListN size list

-- |
-- Finalise the accumulator as vector in reverse order.
{-# INLINE toReverseVector #-}
toReverseVector :: (Vector v a) => Accumulator a -> v a
toReverseVector (Accumulator size list) =
  fromListN size list

-- |
-- Constructor of vectors optimised for appending elements one by one,
-- providing for \(\mathcal{O}(n)\) complexity of the whole construction process.
--
-- Very useful as accumulator in folds.
--
-- Under the hood it is the size counter and a reverse list of elements.
-- When executed, a vector of the according size gets allocated and
-- gets populated with the elements in reverse order
-- (starting from the last element).
data Accumulator a
  = Accumulator !Int ![a]

-- | Create an empty accumulator.
init :: Accumulator a
init = Accumulator 0 []

-- |
-- Add an element to the accumulator.
{-# INLINE add #-}
add :: a -> Accumulator a -> Accumulator a
add head (Accumulator size tail) =
  Accumulator (succ size) (head : tail)

-- |
-- Add a list of elements to the accumulator.
{-# INLINE addList #-}
addList :: [a] -> Accumulator a -> Accumulator a
addList = addFoldable

-- |
-- Add a foldable of elements to the accumulator.
{-# INLINE addFoldable #-}
addFoldable :: (Foldable f) => f a -> Accumulator a -> Accumulator a
addFoldable foldable acc =
  foldl' (\acc a -> add a acc) acc foldable
