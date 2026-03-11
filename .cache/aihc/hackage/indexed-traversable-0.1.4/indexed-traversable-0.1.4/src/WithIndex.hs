{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Safe                   #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module WithIndex where

import Prelude
       (Either (..), Functor (..), Int, Maybe (..), Monad (..), Num (..), error,
       flip, id, seq, snd, ($!), ($), (.))

import Control.Applicative
       (Applicative (..), Const (..), ZipList (..), liftA2, (<$>))
import Control.Applicative.Backwards (Backwards (..))
import Control.Monad.Trans.Identity  (IdentityT (..))
import Control.Monad.Trans.Reader    (ReaderT (..))
import Data.Array                    (Array)
import Data.Foldable                 (Foldable (..))
import Data.Foldable1                (Foldable1 (..))
import Data.Functor.Compose          (Compose (..))
import Data.Functor.Constant         (Constant (..))
import Data.Functor.Identity         (Identity (..))
import Data.Functor.Product          (Product (..))
import Data.Functor.Reverse          (Reverse (..))
import Data.Functor.Sum              (Sum (..))
import Data.IntMap                   (IntMap)
import Data.Ix                       (Ix (..))
import Data.List.NonEmpty            (NonEmpty (..))
import Data.Map                      (Map)
import Data.Monoid                   (Dual (..), Endo (..), Monoid (..))
import Data.Proxy                    (Proxy (..))
import Data.Semigroup                (Semigroup (..))
import Data.Sequence                 (Seq)
import Data.Traversable              (Traversable (..))
import Data.Tree                     (Tree (..))
import Data.Void                     (Void)

import GHC.Generics
       (K1 (..), Par1 (..), Rec1 (..), U1 (..), V1, (:*:) (..), (:+:) (..),
       (:.:) (..))

import qualified Data.Array    as Array
import qualified Data.IntMap   as IntMap
import qualified Data.Map      as Map
import qualified Data.Sequence as Seq

#ifdef MIN_VERSION_base_orphans
import Data.Orphans ()
#endif

import CoerceCompat

-------------------------------------------------------------------------------
-- FunctorWithIndex
-------------------------------------------------------------------------------

-- | A 'Functor' with an additional index.
--
-- Instances must satisfy a modified form of the 'Functor' laws:
--
-- @
-- 'imap' f '.' 'imap' g ≡ 'imap' (\\i -> f i '.' g i)
-- 'imap' (\\_ a -> a) ≡ 'id'
-- @
class Functor f => FunctorWithIndex i f | f -> i where
  -- | Map with access to the index.
  imap :: (i -> a -> b) -> f a -> f b

  default imap :: TraversableWithIndex i f => (i -> a -> b) -> f a -> f b
  imap = imapDefault
  {-# INLINE imap #-}

imapDefault :: TraversableWithIndex i f => (i -> a -> b) -> f a -> f b
-- imapDefault f = runIdentity #. itraverse (\i a -> Identity (f i a))
imapDefault f = runIdentity #. itraverse (Identity #.. f)
{-# INLINE imapDefault #-}

-------------------------------------------------------------------------------
-- FoldableWithIndex
-------------------------------------------------------------------------------

-- | A container that supports folding with an additional index.
class Foldable f => FoldableWithIndex i f | f -> i where
  --
  -- | Fold a container by mapping value to an arbitrary 'Monoid' with access to the index @i@.
  --
  -- When you don't need access to the index then 'foldMap' is more flexible in what it accepts.
  --
  -- @
  -- 'foldMap' ≡ 'ifoldMap' '.' 'const'
  -- @
  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m

  default ifoldMap :: (TraversableWithIndex i f, Monoid m) => (i -> a -> m) -> f a -> m
  ifoldMap = ifoldMapDefault
  {-# INLINE ifoldMap #-}

  -- | A variant of 'ifoldMap' that is strict in the accumulator.
  --
  -- When you don't need access to the index then 'Data.Foldable.foldMap'' is more flexible in what it accepts.
  --
  -- @
  -- 'foldMap'' ≡ 'ifoldMap'' '.' 'const'
  -- @
  ifoldMap' :: Monoid m => (i -> a -> m) -> f a -> m
  ifoldMap' f = ifoldl' (\i acc a -> mappend acc (f i a)) mempty
  {-# INLINE ifoldMap' #-}

  -- | Right-associative fold of an indexed container with access to the index @i@.
  --
  -- When you don't need access to the index then 'Data.Foldable.foldr' is more flexible in what it accepts.
  --
  -- @
  -- 'Data.Foldable.foldr' ≡ 'ifoldr' '.' 'const'
  -- @
  ifoldr   :: (i -> a -> b -> b) -> b -> f a -> b
  ifoldr f z t = appEndo (ifoldMap (Endo #.. f) t) z
  {-# INLINE ifoldr #-}

  -- | Left-associative fold of an indexed container with access to the index @i@.
  --
  -- When you don't need access to the index then 'Data.Foldable.foldl' is more flexible in what it accepts.
  --
  -- @
  -- 'Data.Foldable.foldl' ≡ 'ifoldl' '.' 'const'
  -- @
  ifoldl :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl f z t = appEndo (getDual (ifoldMap (\ i -> Dual #. Endo #. flip (f i)) t)) z
  {-# INLINE ifoldl #-}

  -- | /Strictly/ fold right over the elements of a structure with access to the index @i@.
  --
  -- When you don't need access to the index then 'foldr'' is more flexible in what it accepts.
  --
  -- @
  -- 'foldr'' ≡ 'ifoldr'' '.' 'const'
  -- @
  ifoldr' :: (i -> a -> b -> b) -> b -> f a -> b
  ifoldr' f z0 xs = ifoldl f' id xs z0
    where f' i k x z = k $! f i x z
  {-# INLINE ifoldr' #-}

  -- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
  --
  -- When you don't need access to the index then 'Control.Lens.Fold.foldlOf'' is more flexible in what it accepts.
  --
  -- @
  -- 'Data.Foldable.foldl'' l ≡ 'ifoldl'' l '.' 'const'
  -- @
  ifoldl' :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl' f z0 xs = ifoldr f' id xs z0
    where f' i x k z = k $! f i z x
  {-# INLINE ifoldl' #-}

ifoldMapDefault :: (TraversableWithIndex i f, Monoid m) => (i -> a -> m) -> f a -> m
ifoldMapDefault f = getConst #. itraverse (Const #.. f)
{-# INLINE ifoldMapDefault #-}

-------------------------------------------------------------------------------
-- Foldable1WithIndex
-------------------------------------------------------------------------------

-- | A non-empty container that supports folding with an additional index.
class (Foldable1 f, FoldableWithIndex i f) => Foldable1WithIndex i f | f -> i where
  -- | Map each element of the structure to a semigroup, and combine the results.
  ifoldMap1 :: Semigroup m => (i -> a -> m) -> f a -> m
  ifoldMap1 f = ifoldrMap1 f (\i a m -> f i a <> m)

  -- | A variant of 'ifoldMap1' that is strict in the accumulator.
  ifoldMap1' :: Semigroup m => (i -> a -> m) -> f a -> m
  ifoldMap1' f = ifoldlMap1' f (\i m a -> m <> f i a)

  -- | Generalized 'ifoldr1'.
  ifoldrMap1 :: (i -> a -> b) -> (i -> a -> b -> b) -> f a -> b
  ifoldrMap1 f g xs =
      appFromMaybe (ifoldMap1 (FromMaybe #.. h) xs) Nothing
    where
      h i a Nothing  = f i a
      h i a (Just b) = g i a b

  -- | Generalized 'ifoldl1''.
  ifoldlMap1' :: (i -> a -> b) -> (i -> b -> a -> b) -> f a -> b
  ifoldlMap1' f g xs =
      ifoldrMap1 f' g' xs SNothing
    where
      -- f' :: i -> a -> SMaybe b -> b
      f' i a SNothing  = f i a
      f' i a (SJust b) = g i b a

      -- g' :: i -> a -> (SMaybe b -> b) -> SMaybe b -> b
      g' i a x SNothing  = x $! SJust (f i a)
      g' i a x (SJust b) = x $! SJust (g i b a)

  -- | Generalized 'ifoldl1'.
  ifoldlMap1 :: (i -> a -> b) -> (i -> b -> a -> b) -> f a -> b
  ifoldlMap1 f g xs =
      appFromMaybe (getDual (ifoldMap1 ((Dual . FromMaybe) #.. h) xs)) Nothing
    where
      h i a Nothing  = f i a
      h i a (Just b) = g i b a

  -- | Generalized 'ifoldr1''.
  ifoldrMap1' :: (i -> a -> b) -> (i -> a -> b -> b) -> f a -> b
  ifoldrMap1' f g xs =
      ifoldlMap1 f' g' xs SNothing
    where
      f' i a SNothing  = f i a
      f' i a (SJust b) = g i a b

      g' i bb a SNothing  = bb $! SJust (f i a)
      g' i bb a (SJust b) = bb $! SJust (g i a b)

  {-# MINIMAL ifoldMap1 | ifoldrMap1 #-}

-------------------------------------------------------------------------------
-- TraversableWithIndex
-------------------------------------------------------------------------------

-- | A 'Traversable' with an additional index.
--
-- An instance must satisfy a (modified) form of the 'Traversable' laws:
--
-- @
-- 'itraverse' ('const' 'Identity') ≡ 'Identity'
-- 'fmap' ('itraverse' f) '.' 'itraverse' g ≡ 'Data.Functor.Compose.getCompose' '.' 'itraverse' (\\i -> 'Data.Functor.Compose.Compose' '.' 'fmap' (f i) '.' g i)
-- @
class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) => TraversableWithIndex i t | t -> i where
  -- | Traverse an indexed container.
  --
  -- @
  -- 'itraverse' ≡ 'itraverseOf' 'itraversed'
  -- @
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)

  default itraverse :: (i ~ Int, Applicative f) => (i -> a -> f b) -> t a -> f (t b)
  itraverse f s = snd $ runIndexing (traverse (\a -> Indexing (\i -> i `seq` (i + 1, f i a))) s) 0
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance FunctorWithIndex r ((->) r) where
  imap f g x = f x (g x)
  {-# INLINE imap #-}

instance FunctorWithIndex () Maybe where
  imap f = fmap (f ())
  {-# INLINE imap #-}
instance FoldableWithIndex () Maybe where
  ifoldMap f = foldMap (f ())
  {-# INLINE ifoldMap #-}
instance TraversableWithIndex () Maybe where
  itraverse f = traverse (f ())
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void Proxy where
  imap _ Proxy = Proxy
  {-# INLINE imap #-}

instance FoldableWithIndex Void Proxy where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void Proxy where
  itraverse _ _ = pure Proxy
  {-# INLINE itraverse #-}

instance FunctorWithIndex k ((,) k) where
  imap f (k,a) = (k, f k a)
  {-# INLINE imap #-}

instance FoldableWithIndex k ((,) k) where
  ifoldMap = uncurry'
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex k ((,) k) where
  itraverse f (k, a) = (,) k <$> f k a
  {-# INLINE itraverse #-}

-- | The position in the list is available as the index.
instance FunctorWithIndex Int [] where
  imap f = go 0 where
    go !_ []     = []
    go !n (x:xs) = f n x : go (n + 1) xs
  {-# INLINE imap #-}
instance FoldableWithIndex Int [] where
  ifoldMap = ifoldMapListOff 0
  {-# INLINE ifoldMap #-}
  ifoldr = ifoldrListOff 0
  {-# INLINE ifoldr #-}
  ifoldl' = ifoldl'ListOff 0
instance TraversableWithIndex Int [] where
  itraverse = itraverseListOff 0
  {-# INLINE itraverse #-}

ifoldMapListOff :: Monoid m => Int -> (Int -> a -> m) -> [a] -> m
ifoldMapListOff off f = ifoldrListOff off (\i x acc -> mappend (f i x) acc) mempty

ifoldrListOff :: Int -> (Int -> a -> b -> b) -> b -> [a] -> b
ifoldrListOff !_   _ z []     = z
ifoldrListOff !off f z (x:xs) = f off x (ifoldrListOff (off + 1) f z xs)

ifoldl'ListOff :: Int -> (Int -> b -> a -> b) -> b -> [a] -> b
ifoldl'ListOff !_   _ !z []     = z
ifoldl'ListOff !off f !z (x:xs) = ifoldl'ListOff (off + 1) f (f off z x) xs

-- traverse (uncurry' f) . zip [0..] seems to not work well:
-- https://gitlab.haskell.org/ghc/ghc/-/issues/22673
itraverseListOff :: Applicative f => Int -> (Int -> a -> f b) -> [a] -> f [b]
itraverseListOff !_   _ []     = pure []
itraverseListOff !off f (x:xs) = liftA2 (:) (f off x) (itraverseListOff (off + 1) f xs)

-- TODO: we could experiment with streaming framework
-- imapListFB f xs = build (\c n -> ifoldr (\i a -> c (f i a)) n xs)

-- | Same instance as for @[]@.
instance FunctorWithIndex Int ZipList where
  imap f (ZipList xs) = ZipList (imap f xs)
  {-# INLINE imap #-}
instance FoldableWithIndex Int ZipList where
  ifoldMap f (ZipList xs) = ifoldMap f xs
  {-# INLINE ifoldMap #-}
instance TraversableWithIndex Int ZipList where
  itraverse f (ZipList xs) = ZipList <$> itraverse f xs
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- (former) semigroups
-------------------------------------------------------------------------------

instance FunctorWithIndex Int NonEmpty where
  imap = imapDefault
  {-# INLINE imap #-}
instance FoldableWithIndex Int NonEmpty where
  ifoldMap f (x :| xs) = mappend (f 0 x) (ifoldMapListOff 1 f xs)
  ifoldr f z (x :| xs) = f 0 x (ifoldrListOff 1 f z xs)
  ifoldl' f z (x :| xs) = ifoldl'ListOff 1 f (f 0 z x) xs
  {-# INLINE ifoldMap #-}
instance Foldable1WithIndex Int NonEmpty where
  ifoldMap1 f (x :| xs) = go 1 (f 0 x) xs where
        go _ y [] = y
        go i y (z : zs) = y <> go (i + 1) (f i z) zs
  ifoldMap1' f (x :| xs) = ifoldl'ListOff 1 (\i m y -> m <> f i y) (f 0 x) xs
  ifoldrMap1 f g (x :| xs) = go 0 x xs where
    go i y [] = f i y
    go i y (z : zs) = g i y (go (i + 1) z zs)
  ifoldlMap1' f g (x :| xs) = ifoldl'ListOff 1 g (f 0 x) xs
instance TraversableWithIndex Int NonEmpty where
  itraverse f ~(a :| as) =
    liftA2 (:|) (f 0 a) (itraverseListOff 1 f as)
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- Functors (formely) from transformers
-------------------------------------------------------------------------------

instance FunctorWithIndex () Identity where
  imap f (Identity a) = Identity (f () a)
  {-# INLINE imap #-}

instance FoldableWithIndex () Identity where
  ifoldMap f (Identity a) = f () a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex () Identity where
  itraverse f (Identity a) = Identity <$> f () a
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void (Const e) where
  imap _ (Const a) = Const a
  {-# INLINE imap #-}

instance FoldableWithIndex Void (Const e) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (Const e) where
  itraverse _ (Const a) = pure (Const a)
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void (Constant e) where
  imap _ (Constant a) = Constant a
  {-# INLINE imap #-}

instance FoldableWithIndex Void (Constant e) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (Constant e) where
  itraverse _ (Constant a) = pure (Constant a)
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (i, j) (Compose f g) where
  imap f (Compose fg) = Compose $ imap (\k -> imap (f . (,) k)) fg
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (i, j) (Compose f g) where
  ifoldMap f (Compose fg) = ifoldMap (\k -> ifoldMap (f . (,) k)) fg
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (i, j) (Compose f g) where
  itraverse f (Compose fg) = Compose <$> itraverse (\k -> itraverse (f . (,) k)) fg
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (Sum f g) where
  imap q (InL fa) = InL (imap (q . Left)  fa)
  imap q (InR ga) = InR (imap (q . Right) ga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (Sum f g) where
  ifoldMap q (InL fa) = ifoldMap (q . Left)  fa
  ifoldMap q (InR ga) = ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (Sum f g) where
  itraverse q (InL fa) = InL <$> itraverse (q . Left)  fa
  itraverse q (InR ga) = InR <$> itraverse (q . Right) ga
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (Product f g) where
  imap f (Pair a b) = Pair (imap (f . Left) a) (imap (f . Right) b)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (Product f g) where
  ifoldMap f (Pair a b) = ifoldMap (f . Left) a `mappend` ifoldMap (f . Right) b
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (Product f g) where
  itraverse f (Pair a b) = liftA2 Pair (itraverse (f . Left) a) (itraverse (f . Right) b)
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

instance FunctorWithIndex i m => FunctorWithIndex i (IdentityT m) where
  imap f (IdentityT m) = IdentityT $ imap f m
  {-# INLINE imap #-}

instance FoldableWithIndex i m => FoldableWithIndex i (IdentityT m) where
  ifoldMap f (IdentityT m) = ifoldMap f m
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i m => TraversableWithIndex i (IdentityT m) where
  itraverse f (IdentityT m) = IdentityT <$> itraverse f m
  {-# INLINE itraverse #-}

instance FunctorWithIndex i m => FunctorWithIndex (e, i) (ReaderT e m) where
  imap f (ReaderT m) = ReaderT $ \k -> imap (f . (,) k) (m k)
  {-# INLINE imap #-}

instance FunctorWithIndex i f => FunctorWithIndex i (Backwards f) where
  imap f  = Backwards . imap f . forwards
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Backwards f) where
  ifoldMap f = ifoldMap f . forwards
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Backwards f) where
  itraverse f = fmap Backwards . itraverse f . forwards
  {-# INLINE itraverse #-}

instance FunctorWithIndex i f => FunctorWithIndex i (Reverse f) where
  imap f = Reverse . imap f . getReverse
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Reverse f) where
  ifoldMap f = getDual #. ifoldMap (Dual #.. f) . getReverse
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Reverse f) where
  itraverse f = fmap Reverse . forwards . itraverse (Backwards #.. f) . getReverse
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- array
-------------------------------------------------------------------------------

instance Ix i => FunctorWithIndex i (Array i) where
  imap f arr = Array.listArray (Array.bounds arr) . fmap (uncurry' f) $ Array.assocs arr
  {-# INLINE imap #-}

instance Ix i => FoldableWithIndex i (Array i) where
  ifoldMap f = foldMap (uncurry' f) . Array.assocs
  {-# INLINE ifoldMap #-}

instance Ix i => TraversableWithIndex i (Array i) where
  itraverse f arr = Array.listArray (Array.bounds arr) <$> traverse (uncurry' f) (Array.assocs arr)
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance FunctorWithIndex [Int] Tree where
  imap f (Node a as) = Node (f [] a) $ imap (\i -> imap (f . (:) i)) as
  {-# INLINE imap #-}

instance FoldableWithIndex [Int] Tree where
  ifoldMap f (Node a as) = f [] a `mappend` ifoldMap (\i -> ifoldMap (f . (:) i)) as
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex [Int] Tree where
  itraverse f (Node a as) = liftA2 Node (f [] a) (itraverse (\i -> itraverse (f . (:) i)) as)
  {-# INLINE itraverse #-}
--
-- | The position in the 'Seq' is available as the index.
instance FunctorWithIndex Int Seq where
  imap = Seq.mapWithIndex
  {-# INLINE imap #-}
instance FoldableWithIndex Int Seq where
  ifoldMap = Seq.foldMapWithIndex
  {-# INLINE ifoldMap #-}
  ifoldr = Seq.foldrWithIndex
  {-# INLINE ifoldr #-}
  ifoldl f = Seq.foldlWithIndex (flip f)
  {-# INLINE ifoldl #-}
instance TraversableWithIndex Int Seq where
  itraverse = Seq.traverseWithIndex
  {-# INLINE itraverse #-}

instance FunctorWithIndex Int IntMap where
  imap = IntMap.mapWithKey
  {-# INLINE imap #-}

instance FoldableWithIndex Int IntMap where
  ifoldMap = IntMap.foldMapWithKey
  {-# INLINE ifoldMap #-}
  ifoldr   = IntMap.foldrWithKey
  ifoldl'  = IntMap.foldlWithKey' . flip
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex Int IntMap where
  itraverse = IntMap.traverseWithKey
  {-# INLINE itraverse #-}

instance FunctorWithIndex k (Map k) where
  imap = Map.mapWithKey
  {-# INLINE imap #-}

instance FoldableWithIndex k (Map k) where
  ifoldMap = Map.foldMapWithKey
  {-# INLINE ifoldMap #-}
  ifoldr   = Map.foldrWithKey
  ifoldl'  = Map.foldlWithKey' . flip
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex k (Map k) where
  itraverse = Map.traverseWithKey
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- GHC.Generics
-------------------------------------------------------------------------------

instance FunctorWithIndex Void V1 where
  imap _ v = v `seq` error "imap @V1"
  {-# INLINE imap #-}

instance FoldableWithIndex Void V1 where
  ifoldMap _ v = v `seq` error "ifoldMap @V1"

instance TraversableWithIndex Void V1 where
  itraverse _ v = v `seq` error "itraverse @V1"

instance FunctorWithIndex Void U1 where
  imap _ U1 = U1
  {-# INLINE imap #-}

instance FoldableWithIndex Void U1 where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void U1 where
  itraverse _ U1 = pure U1
  {-# INLINE itraverse #-}

instance FunctorWithIndex () Par1 where
  imap f = fmap (f ())
  {-# INLINE imap #-}

instance FoldableWithIndex () Par1 where
  ifoldMap f (Par1 a) = f () a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex () Par1 where
  itraverse f (Par1 a) = Par1 <$> f () a
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (i, j) (f :.: g) where
  imap q (Comp1 fga) = Comp1 (imap (\k -> imap (q . (,) k)) fga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (i, j) (f :.: g) where
  ifoldMap q (Comp1 fga) = ifoldMap (\k -> ifoldMap (q . (,) k)) fga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (i, j) (f :.: g) where
  itraverse q (Comp1 fga) = Comp1 <$> itraverse (\k -> itraverse (q . (,) k)) fga
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (f :*: g) where
  imap q (fa :*: ga) = imap (q . Left) fa :*: imap (q . Right) ga
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (f :*: g) where
  ifoldMap q (fa :*: ga) = ifoldMap (q . Left) fa `mappend` ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (f :*: g) where
  itraverse q (fa :*: ga) = liftA2 (:*:) (itraverse (q . Left) fa) (itraverse (q . Right) ga)
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (f :+: g) where
  imap q (L1 fa) = L1 (imap (q . Left) fa)
  imap q (R1 ga) = R1 (imap (q . Right) ga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (f :+: g) where
  ifoldMap q (L1 fa) = ifoldMap (q . Left) fa
  ifoldMap q (R1 ga) = ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (f :+: g) where
  itraverse q (L1 fa) = L1 <$> itraverse (q . Left) fa
  itraverse q (R1 ga) = R1 <$> itraverse (q . Right) ga
  {-# INLINE itraverse #-}

instance FunctorWithIndex i f => FunctorWithIndex i (Rec1 f) where
  imap q (Rec1 f) = Rec1 (imap q f)
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Rec1 f) where
  ifoldMap q (Rec1 f) = ifoldMap q f
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Rec1 f) where
  itraverse q (Rec1 f) = Rec1 <$> itraverse q f
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void (K1 i c) where
  imap _ (K1 c) = K1 c
  {-# INLINE imap #-}

instance FoldableWithIndex Void (K1 i c) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (K1 i c) where
  itraverse _ (K1 a) = pure (K1 a)
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- Misc.
-------------------------------------------------------------------------------

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

------------------------------------------------------------------------------
-- Traversed
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
--
-- The argument 'a' of the result should not be used!
newtype Traversed a f = Traversed { getTraversed :: f a }

-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
instance Applicative f => Semigroup (Traversed a f) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)
  {-# INLINE (<>) #-}

instance Applicative f => Monoid (Traversed a f) where
  mempty = Traversed (pure (error "Traversed: value used"))
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- Sequenced
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
--
-- The argument 'a' of the result should not be used!
--
-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
newtype Sequenced a m = Sequenced { getSequenced :: m a }

instance Monad m => Semigroup (Sequenced a m) where
  Sequenced ma <> Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Sequenced a m) where
  mempty = Sequenced (return (error "Sequenced: value used"))
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------------

-- | 'Applicative' composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed'.
newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

  liftA2 f (Indexing ma) (Indexing mb) = Indexing $ \ i -> case ma i of
     (j, ja) -> case mb j of
        ~(k, kb) -> (k, liftA2 f ja kb)
  {-# INLINE liftA2 #-}

-------------------------------------------------------------------------------
-- Strict curry
-------------------------------------------------------------------------------

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b
{-# INLINE uncurry' #-}

-------------------------------------------------------------------------------
-- FromMaybe & SMaybe
-------------------------------------------------------------------------------

-- | Used for foldrMap1 and foldlMap1 definitions
newtype FromMaybe b = FromMaybe { appFromMaybe :: Maybe b -> b }

instance Semigroup (FromMaybe b) where
    FromMaybe f <> FromMaybe g = FromMaybe (f . Just . g)

-- | Strict maybe, used to implement default foldlMap1' etc.
data SMaybe a = SNothing | SJust !a
