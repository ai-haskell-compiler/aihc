{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Matchable(
  -- * Matchable class
  Matchable(..),
  zipzipMatch,
  fmapRecovered,
  eqDefault,
  liftEqDefault,

  -- * Define Matchable by Generic
  Matchable'(), genericZipMatchWith,
) where

import           Control.Applicative

import Data.Functor.Classes ( Eq1 )
import Data.Orphans()

import           Data.Maybe (fromMaybe, isJust)
import           Data.Foldable

import           Data.Functor.Identity
import           Data.Functor.Compose
import           Data.Functor.Product
import           Data.Functor.Sum

import           Data.Tagged
import           Data.Proxy

import           Data.List.NonEmpty     (NonEmpty)

import           Data.Map.Lazy          (Map)
import qualified Data.Map.Lazy          as Map
import           Data.IntMap.Lazy       (IntMap)
import qualified Data.IntMap.Lazy       as IntMap
import qualified Data.IntMap.Merge.Lazy as IntMap
import           Data.Tree              (Tree)
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq

import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector

import           Data.Hashable          (Hashable)
import           Data.HashMap.Lazy      (HashMap)
import qualified Data.HashMap.Lazy      as HashMap

import GHC.Generics
    ( Generic1(..),
      V1,
      U1(..),
      Par1(Par1),
      Rec1(Rec1),
      K1(K1),
      M1(M1),
      type (:+:)(..),
      type (:*:)(..),
      type (:.:)(Comp1) )
import GHC.Generics.Generically ( Generically1(..) )

-- $setup
-- This is required to silence "type defaults" warning, which clutters GHCi
-- output and makes doctests fail.
-- >>> :set -Wno-type-defaults

-- | Containers that allows exact structural matching of two containers.
class (Eq1 t, Functor t) => Matchable t where
  {- |
  Decides if two structures match exactly. If they match, return zipped version of them.

  > zipMatch ta tb = Just tab

  holds if and only if both of

  > ta = fmap fst tab
  > tb = fmap snd tab

  holds. Otherwise, @zipMatch ta tb = Nothing@.

  For example, the type signature of @zipMatch@ on the list Functor @[]@ reads as follows:

  > zipMatch :: [a] -> [b] -> Maybe [(a,b)]

  @zipMatch as bs@ returns @Just (zip as bs)@ if the lengths of two given lists are
  same, and returns @Nothing@ otherwise.

  ==== Example
  >>> zipMatch [1, 2, 3] ['a', 'b', 'c']
  Just [(1,'a'),(2,'b'),(3,'c')]
  >>> zipMatch [1, 2, 3] ['a', 'b']
  Nothing
  -}
  zipMatch :: t a -> t b -> Maybe (t (a,b))
  zipMatch = zipMatchWith (curry Just)

  {- |
  Match two structures. If they match, zip them with given function
  @(a -> b -> Maybe c)@. Passed function can make whole match fail
  by returning @Nothing@.

  A definition of 'zipMatchWith' must satisfy:

      * If there is a pair @(tab, tc)@ such that fulfills all following three conditions,
        then @zipMatchWith f ta tb = Just tc@.

            1. @ta = fmap fst tab@
            2. @tb = fmap snd tab@
            3. @fmap (uncurry f) tab = fmap Just tc@

      * If there are no such pair, @zipMatchWith f ta tb = Nothing@.

  If @t@ is also 'Traversable', the last condition can be dropped and
  the equation can be stated without using @tc@.

  > zipMatchWith f ta tb = traverse (uncurry f) tab
  
  @zipMatch@ can be defined in terms of @zipMatchWith@.
  And if @t@ is also @Traversable@, @zipMatchWith@ can be defined in terms of @zipMatch@.
  When you implement both of them by hand, keep their relation in the way
  the default implementation is.

  > zipMatch             = zipMatchWith (curry pure)
  > zipMatchWith f ta tb = zipMatch ta tb >>= traverse (uncurry f)

  -}
  zipMatchWith :: (a -> b -> Maybe c) -> t a -> t b -> Maybe (t c)

  {-# MINIMAL zipMatchWith #-}

-- | > zipzipMatch = zipMatchWith zipMatch
zipzipMatch
  :: (Matchable t, Matchable u)
  => t (u a)
  -> t (u b)
  -> Maybe (t (u (a, b)))
zipzipMatch = zipMatchWith zipMatch

-- | @Matchable t@ implies @Functor t@.
--   It is not recommended to implement @fmap@ through this function,
--   so it is named @fmapRecovered@ but not @fmapDefault@.
fmapRecovered :: (Matchable t) => (a -> b) -> t a -> t b
fmapRecovered f ta =
  fromMaybe (error "Law-violating Matchable instance") $
    zipMatchWith (\a _ -> Just (f a)) ta ta

-- | @Matchable t@ implies @Eq a => Eq (t a)@.
eqDefault :: (Matchable t, Eq a) => t a -> t a -> Bool
eqDefault = liftEqDefault (==)

-- | @Matchable t@ implies @Eq1 t@.
liftEqDefault :: (Matchable t) => (a -> b -> Bool) -> t a -> t b -> Bool
liftEqDefault eq tx ty =
  let u x y = if x `eq` y then Just () else Nothing
  in isJust $ zipMatchWith u tx ty

-----------------------------------------------

instance Matchable Identity where
  zipMatchWith = genericZipMatchWith

instance (Eq k) => Matchable (Const k) where
  zipMatchWith = genericZipMatchWith

instance (Matchable f, Matchable g) => Matchable (Product f g) where
  zipMatchWith = genericZipMatchWith

instance (Matchable f, Matchable g) => Matchable (Sum f g) where
  zipMatchWith = genericZipMatchWith

instance (Matchable f, Matchable g) => Matchable (Compose f g) where
  zipMatchWith = genericZipMatchWith

instance Matchable Proxy where
  zipMatchWith _ _ _ = Just Proxy

instance Matchable (Tagged t) where
  zipMatchWith = genericZipMatchWith

instance Matchable Maybe where
  zipMatchWith = genericZipMatchWith

instance Matchable [] where
  zipMatchWith = genericZipMatchWith

instance Matchable NonEmpty where
  zipMatchWith = genericZipMatchWith

instance (Eq e) => Matchable ((,) e) where
  zipMatchWith = genericZipMatchWith

instance (Eq e) => Matchable (Either e) where
  zipMatchWith = genericZipMatchWith

instance Matchable Seq where
  zipMatch as bs
    | Seq.length as == Seq.length bs = Just (Seq.zip as bs)
    | otherwise                      = Nothing
  zipMatchWith u as bs
    | Seq.length as == Seq.length bs = unsafeFillIn u as (Data.Foldable.toList bs)
    | otherwise                      = Nothing

instance (Eq k) => Matchable (Map k) where
  zipMatchWith u as bs
    | Map.size as == Map.size bs =
        Map.fromDistinctAscList <$>
          zipMatchWith (zipMatchWith u) (Map.toAscList as) (Map.toAscList bs)
    | otherwise                  = Nothing

instance Matchable IntMap where
  zipMatchWith u as bs
    | IntMap.size as == IntMap.size bs = merger as bs
    | otherwise = Nothing
    where
      miss = IntMap.traverseMissing (\_ _ -> Nothing)
      merger = IntMap.mergeA miss miss (IntMap.zipWithAMatched (const u))

instance Matchable Tree where
  zipMatchWith = genericZipMatchWith

instance Matchable Vector where
  zipMatch as bs
    | Vector.length as == Vector.length bs = Just (Vector.zip as bs)
    | otherwise                            = Nothing

  zipMatchWith u as bs
    | Vector.length as == Vector.length bs = Vector.zipWithM u as bs
    | otherwise                            = Nothing

instance (Eq k, Hashable k) => Matchable (HashMap k) where
  zipMatch as bs
    | HashMap.size as == HashMap.size bs =
        HashMap.traverseWithKey (\k a -> (,) a <$> HashMap.lookup k bs) as
    | otherwise = Nothing
  zipMatchWith u as bs
    | HashMap.size as == HashMap.size bs =
        HashMap.traverseWithKey (\k a -> u a =<< HashMap.lookup k bs) as
    | otherwise = Nothing

instance (Generic1 f, Matchable' (Rep1 f)) => Matchable (Generically1 f) where
  zipMatchWith f (Generically1 x) (Generically1 y) = Generically1 <$> genericZipMatchWith f x y

{- * Generic definition

An instance of Matchable can be implemened through GHC Generics.
As a prerequisite, you need to make your type an instance of 'Functor' and 'Generic1'.
Both of them can be derived using DeriveFunctor and DeriveGeneric extension.

Using 'Generically1' and DerivingVia extension, @Matchable@ instance can be automatically derived.

>>> :set -XDeriveFunctor
>>> :set -XDeriveGeneric
>>> :set -XDerivingVia
>>> :{
  data MyTree label a = Leaf a | Node label [MyTree label a]
    deriving stock (Show, Read, Eq, Ord, Functor, Generic1)
    deriving (Eq1, Matchable) via (Generically1 (MyTree label))
:}

Alternatively, you can use 'genericZipMatchWith' to manually define @zipMatchWith@ method.

> instance (Eq label) => Matchable (MyTree label) where
>   zipMatchWith = genericZipMatchWith
> instance (Eq label) => Eq1 (MyTree label) where
>   liftEq = liftEqDefault

>>> zipMatch (Node "foo" [Leaf 1, Leaf 2]) (Node "foo" [Leaf 'a', Leaf 'b'])
Just (Node "foo" [Leaf (1,'a'),Leaf (2,'b')])
>>> zipMatch (Node "foo" [Leaf 1, Leaf 2]) (Node "bar" [Leaf 'a', Leaf 'b'])
Nothing
>>> zipMatch (Node "foo" [Leaf 1]) (Node "foo" [])
Nothing

-}
class (Functor t, Eq1 t) => Matchable' t where
  zipMatchWith' :: (a -> b -> Maybe c) -> t a -> t b -> Maybe (t c)

-- | zipMatchWith via Generics.
genericZipMatchWith
  :: (Generic1 t, Matchable' (Rep1 t))
  => (a -> b -> Maybe c)
  -> t a
  -> t b
  -> Maybe (t c)
genericZipMatchWith u ta tb = to1 <$> zipMatchWith' u (from1 ta) (from1 tb)
{-# INLINABLE genericZipMatchWith #-}

instance Matchable' V1 where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' _ a _ = case a of { }

instance Matchable' U1 where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' _ _ _ = pure U1

instance Matchable' Par1 where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' u (Par1 a) (Par1 b) = Par1 <$> u a b

instance Matchable f => Matchable' (Rec1 f) where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' u (Rec1 fa) (Rec1 fb) = Rec1 <$> zipMatchWith u fa fb

instance (Eq c) => Matchable' (K1 i c) where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' _ (K1 ca) (K1 cb)
    = if ca == cb then pure (K1 ca) else empty

instance Matchable' f => Matchable' (M1 i c f) where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' u (M1 fa) (M1 fb) = M1 <$> zipMatchWith' u fa fb

instance (Matchable' f, Matchable' g) => Matchable' (f :+: g) where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' u (L1 fa) (L1 fb) = L1 <$> zipMatchWith' u fa fb
  zipMatchWith' u (R1 ga) (R1 gb) = R1 <$> zipMatchWith' u ga gb
  zipMatchWith' _ _       _       = empty

instance (Matchable' f, Matchable' g) => Matchable' (f :*: g) where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' u (fa :*: ga) (fb :*: gb) =
    liftA2 (:*:) (zipMatchWith' u fa fb) (zipMatchWith' u ga gb)

instance (Matchable f, Matchable' g) => Matchable' (f :.: g) where
  {-# INLINABLE zipMatchWith' #-}
  zipMatchWith' u (Comp1 fga) (Comp1 fgb) =
    Comp1 <$> zipMatchWith (zipMatchWith' u) fga fgb

-- Utility functions

unsafeFillIn :: (Traversable f) => (a -> b -> Maybe c) -> f a -> [b] -> Maybe (f c)
unsafeFillIn u as bs = fst <$> runFillIn (traverse (useOne u) as) bs

-- Just a @StateT [b] Maybe@ but avoids to depend on transformers
newtype FillIn b a = FillIn { runFillIn :: [b] -> Maybe (a, [b]) }
  deriving (Functor)

instance Applicative (FillIn b) where
  pure a = FillIn $ \bs -> Just (a, bs)
  FillIn fx <*> FillIn fy = FillIn $ \bs ->
    fx bs >>= \(x, bs') ->
    fy bs' >>= \(y, bs'') -> Just (x y, bs'')

useOne :: (a -> b -> Maybe c) -> a -> FillIn b c
useOne u a = FillIn $ \bs -> case bs of
  [] -> Nothing
  (b:bs') -> u a b >>= \c -> Just (c, bs')

