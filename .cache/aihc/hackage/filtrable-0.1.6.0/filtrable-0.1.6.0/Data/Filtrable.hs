{-# LANGUAGE CPP #-}

-- | See 'Filtrable'.
module Data.Filtrable
  ( Filtrable (..)
  , (<$?>), (<*?>)
  , nub, nubBy, nubOrd, nubOrdBy
  ) where

import Prelude hiding (filter)

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad
import qualified Control.Monad.Trans.State as M
import Data.Bool (bool)
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum
import Data.Proxy
import Data.Traversable

#ifdef MIN_VERSION_containers
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
#endif

import qualified Data.Set.Private as Set

-- | Class of filtrable containers, i.e. containers we can map over while selectively dropping elements.
--
-- Laws:
--
-- * @'mapMaybe' 'Just' = id@
--
-- * @'mapMaybe' f = 'catMaybes' ∘ 'fmap' f@
--
-- * @'catMaybes' = 'mapMaybe' id@
--
-- * @'filter' f = 'mapMaybe' (\\ x -> 'bool' 'Nothing' ('Just' x) (f x))@
--
-- * @'mapMaybe' g . 'mapMaybe' f = 'mapMaybe' (g '<=<' f)@
--
--   Laws if @'Foldable' f@:
--
-- * @'foldMap' g . 'filter' f = 'foldMap' (\\ x -> 'bool' 'mempty' (g x) (f x))@
class Functor f => Filtrable f where
    {-# MINIMAL mapMaybe | catMaybes #-}

    -- | Map the container with the given function, dropping the elements for which it returns 'Nothing'.
    mapMaybe :: (a -> Maybe b) -> f a -> f b
    mapMaybe f = catMaybes . fmap f

    -- | @'catMaybes' = 'mapMaybe' 'id'@
    catMaybes :: f (Maybe a) -> f a
    catMaybes = mapMaybe id

    -- | Drop the elements for which the given predicate is 'False'.
    filter :: (a -> Bool) -> f a -> f a
    filter f = mapMaybe ((<$) <*> guard . f)

    -- | Traverse the container with the given function, dropping the elements for which it returns 'Nothing'.
    mapMaybeA :: (Traversable f, Applicative p) => (a -> p (Maybe b)) -> f a -> p (f b)
    mapMaybeA f xs = catMaybes <$> traverse f xs

    -- | Drop the elements for which the given predicate is 'False'.
    filterA :: (Traversable f, Applicative p) => (a -> p Bool) -> f a -> p (f a)
    filterA f = mapMaybeA (\ x -> (x <$) . guard <$> f x)

    -- | Map the container with the given function, collecting the 'Left's and the 'Right's separately.
    mapEither :: (a -> Either b c) -> f a -> (f b, f c)
    mapEither f = (,) <$> mapMaybe (either Just (pure Nothing) . f)
                      <*> mapMaybe (either (pure Nothing) Just . f)

    -- | Traverse the container with the given function, collecting the 'Left's and the 'Right's separately.
    mapEitherA :: (Traversable f, Applicative p) => (a -> p (Either b c)) -> f a -> p (f b, f c)
    mapEitherA f = liftA2 (,) <$> mapMaybeA (fmap (Just `either` pure Nothing) . f)
                              <*> mapMaybeA (fmap (pure Nothing `either` Just) . f)

    -- | @'partitionEithers' = 'mapEither' 'id'@
    partitionEithers :: f (Either a b) -> (f a, f b)
    partitionEithers = mapEither id

instance Filtrable [] where
    mapMaybe f = foldr (maybe id (:) . f) []

    mapMaybeA _ [] = pure []
    mapMaybeA f (x:xs) = maybe id (:) <$> f x <*> mapMaybeA f xs

instance Filtrable Maybe where
    mapMaybe = (=<<)
    catMaybes = join

instance Filtrable Proxy where
    mapMaybe _ Proxy = Proxy

instance Filtrable (Const a) where
    mapMaybe _ (Const x) = Const x

instance (Filtrable f, Filtrable g) => Filtrable (Product f g) where
    mapMaybe f (Pair as bs) = Pair (mapMaybe f as) (mapMaybe f bs)

instance (Filtrable f, Filtrable g) => Filtrable (Sum f g) where
    mapMaybe f = \ case
        InL as -> InL (mapMaybe f as)
        InR bs -> InR (mapMaybe f bs)

instance (Functor f, Filtrable g) => Filtrable (Compose f g) where
    mapMaybe f = Compose . (fmap . mapMaybe) f . getCompose

instance Filtrable f => Filtrable (Backwards f) where
    mapMaybe f = Backwards . mapMaybe f . forwards

instance Filtrable f => Filtrable (Reverse f) where
    mapMaybe f = Reverse . mapMaybe f . getReverse

infixl 4 <$?>, <*?>

-- | Infix synonym of 'mapMaybe'
(<$?>) :: Filtrable f => (a -> Maybe b) -> f a -> f b
(<$?>) = mapMaybe

-- | @f '<*?>' a = 'catMaybes' (f '<*>' a)@
(<*?>) :: (Applicative p, Filtrable p) => p (a -> Maybe b) -> p a -> p b
f <*?> a = catMaybes (f <*> a)

-- | \(\mathcal{O}(n^2)\)
-- Delete all but the first copy of each element, special case of 'nubBy'.
nub :: (Filtrable f, Traversable f, Eq a) => f a -> f a
nub = nubBy (==)

-- | \(\mathcal{O}(n^2)\)
-- Delete all but the first copy of each element, with the given relation.
nubBy :: (Filtrable f, Traversable f) => (a -> a -> Bool) -> f a -> f a
nubBy eq = fmap (flip M.evalState []) . filterA $ \ a -> do
    as <- M.get
    let b = all (not . eq a) as
    b <$ when b (M.modify (a:))

-- | \(\mathcal{O}(n\;\mathrm{log}\;n)\)
-- Delete all but the first copy of each element, special case of 'nubOrdBy'.
nubOrd :: (Filtrable f, Traversable f, Ord a) => f a -> f a
nubOrd = nubOrdBy compare

-- | \(\mathcal{O}(n\;\mathrm{log}\;n)\)
-- Delete all but the first copy of each element, with the given relation.
nubOrdBy :: (Filtrable f, Traversable f) => (a -> a -> Ordering) -> f a -> f a
nubOrdBy compare = fmap (flip M.evalState Set.empty) . filterA $ \ a -> M.state $ \ as ->
    case Set.insertBy' compare a as of
        Nothing -> (False, as)
        Just as' -> (True, as')

#ifdef MIN_VERSION_containers
instance Filtrable IntMap where
    mapMaybe = IntMap.mapMaybe
    mapEither = IntMap.mapEither
    filter = IntMap.filter

instance Ord k => Filtrable (Map k) where
    mapMaybe = Map.mapMaybe
    mapEither = Map.mapEither
    filter = Map.filter

instance Filtrable Seq where
    mapMaybe f = go
      where
        go = \ case
            Seq.Empty -> Seq.Empty
            a Seq.:<| as -> maybe id (Seq.:<|) (f a) (go as)
#endif
