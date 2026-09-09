module Data.Functor
  ( Functor (..),
    (<$>),
    (<&>),
    ($>),
    unzip,
    void,
  )
where

import Prelude (Functor (..), const, fst, snd, (<$), (<$>))

-- | Flipped '<$>'.
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
functor <&> function = fmap function functor

infixl 1 <&>

-- | Replace every result of @functor@ with @value@.
($>) :: (Functor f) => f a -> b -> f b
functor $> value = value <$ functor

infixl 4 $>

-- | Separate a functor of pairs into a pair of functors.
unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip pairs = (fmap fst pairs, fmap snd pairs)

-- | Discard the result of @functor@.
void :: (Functor f) => f a -> f ()
void = fmap (const ())
