{-# LANGUAGE CPP #-}

-- | A simple data structure helping us ask questions of the following
-- sort: "does all this data have the same /BLANK/ and if so what is
-- it?"
--
-- For example:
--
-- > doTheseHaveTheSameLength :: [String] -> String
-- > doTheseHaveTheSameLength l = case foldMap (Somebody . length) of
-- >   Somebody n -> "They all have length " <> show n
-- >   Nobody     -> "The lengths differ"
-- >   Anybody    -> "You didn't give me any strings"
--
-- This can of course be done with `Maybe (Maybe x)` instead, but
-- doing so runs the risk of getting confused: which is `Nothing` and
-- which is `Just Nothing`?
--
-- Unfortunately, there are two `Applicative` instances.
--
-- One is easy to motivate intrinsically. If we think of `Anybody` as
-- an empty list, `Somebody` as a singleton list, and `Nobody` as a
-- multi-element list, and think of the applicative instance on as
-- corresponding to the cartesian product, then we get an
-- `Applicative` instance with
--
-- > Anybody <*> Anybody = Anybody
-- > Anybody <*> Nobody = Nobody
--
-- This however cannot possibly correspond to a `Monad` instance (if
-- the first argument of `>>=` is Anybody, there's no way of
-- inspecting the second). We thus choose another, which does.
module Data.Agreement (
  Agreement(..),
  getSomebody,
  ) where

#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
#else
import Control.Applicative (liftA2)
#endif

import Data.Semigroup (Semigroup(..),
                       stimesIdempotentMonoid)

-- | We have the following constructors:
--
--   * `Somebody` is a consistent choice of an element.
--
--   * `Nobody` is an inconsistent choice.
--
--   * `Anybody` is a failure to make any choice.
data Agreement a = Anybody | Somebody a | Nobody
  deriving (Eq, Ord, Show)

-- | This picks out consistent choices as `Just`.
getSomebody :: Agreement a -> Maybe a
getSomebody (Somebody x) = Just x
getSomebody _ = Nothing

instance Functor Agreement where
  fmap _ Anybody      = Anybody
  fmap f (Somebody x) = Somebody (f x)
  fmap _ Nobody       = Nobody

-- | Not the only possible instance: see introduction
instance Applicative Agreement where
  pure = Somebody
  Nobody     <*> _ = Nobody
  Anybody    <*> _ = Anybody
  Somebody f <*> x = f <$> x
  liftA2 _ Nobody       _ = Nobody
  liftA2 _ Anybody      _ = Anybody
  liftA2 f (Somebody x) y = f x <$> y

instance Monad Agreement where
  return = pure
  Nobody     >>= _ = Nobody
  Anybody    >>= _ = Anybody
  Somebody x >>= f = f x

instance (Eq a) => Semigroup (Agreement a) where
  Nobody     <> _          = Nobody
  Anybody    <> x          = x
  Somebody _ <> Nobody     = Nobody
  Somebody x <> Anybody    = Somebody x
  Somebody x <> Somebody y
    | x == y               = Somebody x
    | otherwise            = Nobody
  stimes = stimesIdempotentMonoid

instance (Eq a) => Monoid (Agreement a) where
  mempty = Anybody
