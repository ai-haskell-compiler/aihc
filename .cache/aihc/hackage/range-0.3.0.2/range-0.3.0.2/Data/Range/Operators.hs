module Data.Range.Operators where

import Data.Range.Data

-- | Mathematically equivalent to @[x, y]@.
--
-- @x +=+ y@ is the short version of @SpanRange (Bound x Inclusive) (Bound y Inclusive)@
(+=+) :: a -> a -> Range a
(+=+) x y = SpanRange (Bound x Inclusive) (Bound y Inclusive)

-- | Mathematically equivalent to @[x, y)@.
--
-- @x +=* y@ is the short version of @SpanRange (Bound x Inclusive) (Bound y Exclusive)@
(+=*) :: a -> a -> Range a
(+=*) x y = SpanRange (Bound x Inclusive) (Bound y Exclusive)

-- | Mathematically equivalent to @(x, y]@.
--
-- @x *=+ y@ is the short version of @SpanRange (Bound x Exclusive) (Bound y Inclusive)@
(*=+) :: a -> a -> Range a
(*=+) x y = SpanRange (Bound x Exclusive) (Bound y Inclusive)

-- | Mathematically equivalent to @(x, y)@.
--
-- @x *=* y@ is the short version of @SpanRange (Bound x Exclusive) (Bound y Exclusive)@
(*=*) :: a -> a -> Range a
(*=*) x y = SpanRange (Bound x Exclusive) (Bound y Exclusive)

-- | Mathematically equivalent to @[x, Infinity)@.
--
-- @lbi x@ is the short version of @LowerBoundRange (Bound x Inclusive)@
lbi :: a -> Range a
lbi x = LowerBoundRange (Bound x Inclusive)

-- | Mathematically equivalent to @(x, Infinity)@.
--
-- @lbe x@ is the short version of @LowerBoundRange (Bound x Exclusive)@
lbe :: a -> Range a
lbe x = LowerBoundRange (Bound x Exclusive)

-- | Mathematically equivalent to @(Infinity, x]@.
--
-- @ubi x@ is the short version of @UpperBoundRange (Bound x Inclusive)@
ubi :: a -> Range a
ubi x = UpperBoundRange (Bound x Inclusive)

-- | Mathematically equivalent to @(Infinity, x)@.
--
-- @ube x@ is the short version of @UpperBoundRange (Bound x Exclusive)@
ube :: a -> Range a
ube x = UpperBoundRange (Bound x Exclusive)

-- | Shorthand for the `InfiniteRange`
inf :: Range a
inf = InfiniteRange