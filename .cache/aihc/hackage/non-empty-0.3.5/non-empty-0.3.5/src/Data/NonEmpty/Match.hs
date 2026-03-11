{- |
Generalized version of utility-ht:"Data.List.Match".
-}
module Data.NonEmpty.Match (take, replicate) where

import qualified Data.NonEmpty.Class as C

import Control.Functor.HT (void, )

import Prelude hiding (take, replicate, )


{- | Make a list as long as another one -}
{-
@flip (zipWith const)@ is not as lazy,
e.g. would be @take [] undefined = undefined@,
but it should be @take [] undefined = []@.
-}
take :: (C.Zip f) => f b -> f a -> f a
take = C.zipWith (flip const)


{- |
Check whether two lists with different element types have equal length.
It is equivalent to @length xs == length ys@ but more efficient.
-}
{-
I'd prefer a type constructor class Eq
-}
_equalLength :: (Functor f, Eq (f ())) => f a -> f b -> Bool
_equalLength xs ys =
   void xs == void ys

{- |
Compare the length of two lists over different types.
It is equivalent to @(compare (length xs) (length ys))@
but more efficient.
-}
{-
I'd prefer a type constructor class Ord
-}
_compareLength :: (Functor f, Ord (f ())) => f a -> f b -> Ordering
_compareLength xs ys =
   compare (void xs) (void ys)


{- |
the same as @($>)@
-}
replicate :: (Functor f) => f a -> b -> f b
replicate xs y = fmap (const y) xs
