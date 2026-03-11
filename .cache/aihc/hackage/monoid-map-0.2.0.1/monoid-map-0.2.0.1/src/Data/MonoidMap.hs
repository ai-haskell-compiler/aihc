-- | This module contains a newtype wrapper around 'Data.Map.Map' that has a
-- correct 'Group' instance compared to the one for
-- 'Data.Map.Monoidal.MonoidalMap', in that it has a unique neutral element.
-- This comes with different constraints on the parameters (check the instances
-- for 'Semigroup' and 'Monoid' of the corresponding data structures if you're
-- interested).
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-} -- For (DecidablyEmpty (QueryResult q), Ord k, Query q) => Query (MonoidMap k q)
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
module Data.MonoidMap where

import Data.Semigroup.Commutative
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as Map
import Reflex (Query, QueryResult, crop, Group(..))
import Data.Monoid.DecidablyEmpty
import GHC.TypeLits
import Witherable

-- | Newtype wrapper around Data.Map.Monoidal.MonoidalMap
newtype MonoidMap k v = MonoidMap { unMonoidMap :: MonoidalMap k v }
  deriving (Show, Eq, Ord, Foldable)

instance TypeError (Text "Use mapMonoidMap instead of fmap; MonoidMap is not a Functor because mempty values would need to be deleted, and Functors cannot change the shape of a datastructure") => Functor (MonoidMap k) where
  fmap = error "Impossible"

emptyToNothing :: DecidablyEmpty a => a -> Maybe a
emptyToNothing a = if isEmpty a then Nothing else Just a

mapMonoidMap :: DecidablyEmpty b => (a -> b) -> MonoidMap k a -> MonoidMap k b
mapMonoidMap f (MonoidMap a) = MonoidMap $ mapMaybe (emptyToNothing . f) a

traverseMonoidMap :: (Ord k, DecidablyEmpty b, Applicative f) => (a -> f b) -> MonoidMap k a -> f (MonoidMap k b)
traverseMonoidMap f (MonoidMap a) = MonoidMap <$> wither (fmap emptyToNothing . f) a

instance (Ord k, DecidablyEmpty v) => DecidablyEmpty (MonoidMap k v) where
  isEmpty (MonoidMap m) = Map.null m

-- | Convert a MonoidalMap into a MonoidMap
monoidMap :: (Ord k, DecidablyEmpty v) => MonoidalMap k v -> MonoidMap k v
monoidMap = MonoidMap . Map.filter (not . isEmpty)

instance (DecidablyEmpty (QueryResult q), Ord k, Query q) => Query (MonoidMap k q) where
  type QueryResult (MonoidMap k q) = MonoidMap k (QueryResult q)
  crop (MonoidMap q) (MonoidMap qr) =
    -- This assumes that the query result of a null query should be null
    monoidMap $ Map.intersectionWith crop q qr

instance (Monoid a, DecidablyEmpty a, Ord k) => Semigroup (MonoidMap k a) where
  MonoidMap a <> MonoidMap b =
    let combine _ a' b' =
          let c = a' `mappend` b'
          in if isEmpty c
               then Nothing
               else Just c
    in MonoidMap $ Map.mergeWithKey combine id id a b

instance (Ord k, DecidablyEmpty a) => Monoid (MonoidMap k a) where
  mempty = MonoidMap Map.empty
  mappend = (<>)

instance (Ord k, DecidablyEmpty a, Group a) => Group (MonoidMap k a) where
  negateG (MonoidMap a) = MonoidMap $ fmap negateG a

instance (Ord k, DecidablyEmpty a, Group a, Commutative a) => Commutative (MonoidMap k a)
