-- | Utilities for the 'Functor' class.
module Data.Functor
  ( Functor (..),
    (<$>),
    (<&>),
    ($>),
    unzip,
    void,
  )
where

import Prelude (Functor (..), const, flip, fst, snd, (<$>))

infixl 1 <&>

-- | 'fmap' with its arguments flipped.
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

infixl 4 $>

-- | Replace every value with the given one.
($>) :: (Functor f) => f a -> b -> f b
functor $> value = fmap (const value) functor

-- | Split a functor of pairs into a pair of functors.
unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip pairs = (fmap fst pairs, fmap snd pairs)

-- | Discard the result of a functor.
void :: (Functor f) => f a -> f ()
void = fmap (const ())
