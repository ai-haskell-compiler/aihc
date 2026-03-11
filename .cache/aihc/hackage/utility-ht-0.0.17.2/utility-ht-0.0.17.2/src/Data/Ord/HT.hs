module Data.Ord.HT where

import Data.Function.HT (compose2, )

{-# INLINE comparing #-}
comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing = compose2 compare

{- |
@limit (lower,upper) x@ restricts @x@ to the range from @lower@ to @upper@.
Don't expect a sensible result for @lower>upper@.

Called @clamp@ elsewhere.
-}
{-# INLINE limit #-}
limit :: (Ord a) => (a,a) -> a -> a
limit (l,u) = max l . min u

{- |
@limit (lower,upper) x@ checks whether @x@ is in the range from @lower@ to @upper@.
Don't expect a sensible result for @lower>upper@.
-}
{-# INLINE inRange #-}
inRange :: (Ord a) => (a,a) -> a -> Bool
inRange (l,u) x  =  l<=x && x<=u
