{-# LANGUAGE
  FlexibleContexts,
  StandaloneDeriving,
  TypeFamilies,
  UndecidableInstances #-}

-- | 'Interval' implements 'Shift'.
module DiffLoc.Interval
  ( Interval(..)
  , isZeroLength
  , Replace(..)
  ) where

import Prelude hiding ((+))
import DiffLoc.Shift
import DiffLoc.Unsafe ((.-.))

-- $setup
-- >>> import DiffLoc
-- >>> import DiffLoc.Test
-- >>> import Test.QuickCheck

-- Nicer looking formulas this way.

infixl 6 +
(+) :: Semigroup a => a -> a -> a
(+) = (<>)

-- | @(i ':..' n)@ represents a span of text between index @i@ and index @i+n@.
--
-- The type of indices @p@ is expected to be an instance of 'Amor'.
--
-- The length @n@ in an interval @(i :.. n)@ may be zero.
--
-- The elements of the interval can be thought of as indexing the interstices
-- /between/ characters. A span of length zero is a single interstice between
-- two characters, where some chunk of text may be inserted.
--
-- Example: drawing of @1 :.. 2@ in "abcde".
--
-- >  a b c d e
-- > 0 1 2 3 4 5
-- >   ^b+c+ length = 2
-- >   ^
-- >   ^ start = 1
data Interval p = !p :.. !(Trans p)

infixl 3 :..

-- | Does the interval have length zero?
isZeroLength :: (Eq (Trans p), Monoid (Trans p)) => Interval p -> Bool
isZeroLength (_ :.. n) = n == mempty

deriving instance (Eq p, Eq (Trans p)) => Eq (Interval p)
deriving instance (Show p, Show (Trans p)) => Show (Interval p)

instance Amor p => BlockOrder (Interval p) where
  precedes (i :.. n) (j :.. _) = i .+ n <= j
  distantlyPrecedes (i :.. n) (j :.. _) = i .+ n < j

-- | A minimalistic representation of text replacements.
--
-- A replacement @'Replace' i n m@ is given by a start location @i@, the length
-- @n@ of the interval to replace (source) and the length @m@ of its
-- replacement (target).
-- This representation does not keep track of the actual data being inserted.
--
-- This may overapproximate the underlying text replacement,
-- with intervals being wider than necessary.
-- For example, the transformation from "abc" to "ac" could be represented
-- by @mkReplace 1 1 0@ (replace "b" with "" at location 1), and also by
-- @mkReplace 0 2 1@ (replace "ab" with "a" at location 0).
--
-- > source   abc   abc
-- >      -    b    ab
-- >      +         a
-- > target   a c   a c
--
-- Insertions are replacements with source intervals of length zero.
-- Deletions are replacements with target intervals of length zero.
data Replace p = Replace !p !(Trans p) !(Trans p)

deriving instance (Eq p, Eq (Trans p)) => Eq (Replace p)
deriving instance (Show p, Show (Trans p)) => Show (Replace p)

-- | The composition of two replacements @l <> r@ represents the replacement @r@
-- followed by @l@, as one replacement of an span that contains both @r@ and @l@.
--
-- The right-to-left order of composition has the nice property that when
-- @l `'precedes` r@, @l <> r@ can also be viewed intuitively as performing @l@ and
-- @r@ simultaneously.
--
-- === Properties
--
-- prop> (x <> y) <> z === x <> (y <> z :: Replace (Plain Int))
instance Amor p => Semigroup (Replace p) where
  Replace li ln lm <> Replace ri rn rm
    | li .+ ln <= ri
      -- Disjoint, l on the left.
      --
      -- Before:
      -- > |---l---|       |---r---|
      -- > li      li+ln   ri      ri+rn
      --
      -- After both replacements (r first),
      -- with ld = lm-ln
      --
      -- > |---l---|       |---r---|
      -- > li      li+lm   ri+ld   ri+rm+ld
      --
    = Replace li ((ri .+ rn) .-. li) (lm + (ri .-. (li .+ ln)) + rm)

    | li <= ri
      -- l straddles the left end of r
      --
      -- Note that the indices in l should be interpreted
      -- as indices after r.
      -- After replacing r, the replaced span r and the to-be-replaced
      -- span l look like this:
      --
      -- >       |------r----|
      -- > |----l-----|
      -- > li    ri   li+ln  ri+rm
      --
      -- or this:
      --
      -- >      |--r--|
      -- > |-------l----------|
      -- > li   ri    ri+rm   li+ln
      --
    = let (n, m) = if li .+ ln < ri .+ rm
                   then ((ri .+ rn) .-. li, lm + ((ri .+ rm) .-. (li .+ ln)))
                   else ((ri .-. li) + rn + ((li .+ ln) .-. (ri .+ rm)), lm)
      in Replace li n m

    | li < ri .+ rm
      -- r straddles the left end of l
      --
      -- > |----r-----|
      -- >       |------l----|
      -- > ri    li   ri+rm  li+ln
      --
      -- or
      --
      -- > |-------r----------|
      -- >      |--l--|
      -- > ri   li    li+ln   ri+rm
      --
    = let (n, m) = if ri .+ rm < li .+ ln
                   then (rn + ((li .+ ln) .-. (ri .+ rm)), (li .+ lm) .-. ri)
                   else (rn, (li .-. ri) + lm + ((ri .+ rm) .-. (li .+ ln)))
      in Replace ri n m

    | otherwise
      --
      -- > |---r---|       |---l---|
      -- > ri      rm      li      ln
      --
    = Replace ri (rn + (li .-. (ri .+ rm)) + ln) ((li .+ lm) .-. ri)

instance Amor p => Shift (Replace p) where
  type Block (Replace p) = Interval p
  dual (Replace i n m) = Replace i m n

  src (Replace i n _) = i :.. n

  shiftBlock (Replace i n m) jp@(j :.. p)
    | j .+ p <= i = Just jp
    | i .+ n <= j = Just (i .+ (m + (j .-. (i .+ n))) :.. p)
    | otherwise = Nothing

  shiftR (Replace i n m) jpq@(Replace j p q)
    | j .+ p <= i = Just jpq
    | i .+ n <= j = Just (Replace (i .+ (m + (j .-. (i .+ n)))) p q)
    | otherwise = Nothing
