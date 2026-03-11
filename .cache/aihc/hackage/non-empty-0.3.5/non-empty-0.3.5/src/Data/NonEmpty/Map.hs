module Data.NonEmpty.Map (
   T,
   insert,
   insertWith,
   singleton,
   member,
   size,
   elems,
   keys,
   keysSet,
   lookup,
   delete,
   minViewWithKey,
   maxViewWithKey,
   fromList,
   fromListWith,
   fromAscList,
   toAscList,
   fetch,
   flatten,
   union,
   unionLeft,
   unionRight,
   unionWith,
   unionLeftWith,
   unionRightWith,
   map,
   mapWithKey,
   ) where

import qualified Data.NonEmpty.Set as NonEmptySet
import qualified Data.NonEmpty.Class as C
import qualified Data.NonEmpty as NonEmpty

import qualified Data.Map as Map
import Data.Map (Map, )

import Control.Monad (mzero, )
import Control.Applicative (liftA2, liftA3)
import Control.DeepSeq (NFData, rnf, )
import Data.Traversable (Traversable, traverse, )
import Data.Foldable (Foldable, foldMap, )
import Data.Monoid (mappend, )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple.HT (forcePair, mapSnd, )
import Data.Ord.HT (comparing, )

import qualified Test.QuickCheck as QC

import Prelude hiding (map, lookup, )


{- $setup
>>> import qualified Data.NonEmpty.Map as NonEmptyMap
>>> import qualified Data.NonEmpty as NonEmpty
>>> import qualified Data.Map as Map
>>> import qualified Test.QuickCheck as QC
>>>
>>> forAllMap :: (QC.Testable test) => (Map.Map Int String -> test) -> QC.Property
>>> forAllMap = QC.forAll (fmap Map.fromList QC.arbitrary)
>>>
>>> forAllNonEmptyMap :: (QC.Testable test) => (NonEmptyMap.T Int String -> test) -> QC.Property
>>> forAllNonEmptyMap = QC.forAll (fmap NonEmptyMap.fromList QC.arbitrary)
-}

{-
The first field will always contain the smallest element.
-}
data T k a = Cons (k, a) (Map k a)
   deriving (Eq, Ord)

instance (Show k, Show a) => Show (T k a) where
   showsPrec p xs =
      showParen (p>10) $
         showString "NonEmptyMap.fromList " .
         showsPrec 11 (toAscList xs)

instance (Ord k) => Functor (T k) where
   fmap = map

instance (Ord k) => Foldable (T k) where
   foldMap f (Cons x xs) = mappend (f (snd x)) (foldMap f xs)

instance (Ord k) => Traversable (T k) where
   traverse f (Cons x xs) =
      liftA2 Cons (fmap ((,) (fst x)) $ f (snd x)) (traverse f xs)

instance (NFData k, NFData a) => NFData (T k a) where
   rnf = C.rnf

instance (NFData k) => C.NFData (T k) where
   rnf (Cons x xs) = rnf (x, C.rnf xs)

instance (QC.Arbitrary k, Ord k, QC.Arbitrary a) => QC.Arbitrary (T k a) where
   arbitrary = C.arbitrary
   shrink = C.shrink

instance (QC.Arbitrary k, Ord k) => C.Arbitrary (T k) where
   arbitrary = liftA3 insert QC.arbitrary QC.arbitrary QC.arbitrary
   shrink = mapMaybe fetch . QC.shrink . flatten

instance (QC.Arbitrary k, Ord k) => C.Gen (T k) where
   genOf gen = liftA3 insert QC.arbitrary gen $ C.genOf gen


-- | prop> \k a -> forAllMap $ \m -> Map.insert k a m == NonEmptyMap.flatten (NonEmptyMap.insert k a m)
insert :: Ord k => k -> a -> Map k a -> T k a
insert = curry $ insertGen Map.insert fst

-- | prop> \k a -> forAllMap $ \m -> Map.insertWith (++) k a m == NonEmptyMap.flatten (NonEmptyMap.insertWith (++) k a m)
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> T k a
insertWith f = curry $ insertGen (Map.insertWith f) (applyFst f)

applyFst :: (a -> a -> a) -> ((k,a), (k,a)) -> (k,a)
applyFst f ((k,a0),(_,a1)) = (k, f a0 a1)

insertRight :: Ord k => (k,a) -> Map k a -> T k a
insertRight = insertGen (Map.insertWith $ flip const) snd

insertRightWith :: Ord k => (a -> a -> a) -> (k,a) -> Map k a -> T k a
insertRightWith f =
   insertGen (Map.insertWith $ flip f) $ \((_,a0),(k,a1)) -> (k, f a1 a0)

insertGen ::
   Ord k =>
   (k -> a -> Map k a -> Map k a) ->
   (((k,a),(k,a)) -> (k,a)) ->
   (k,a) -> Map k a -> T k a
insertGen ins select y xt =
   uncurry Cons $
   fromMaybe (y, xt) $ do
      (x,xs) <- Map.minViewWithKey xt
      case comparing fst y x of
         GT -> return (x, uncurry ins y xs)
         EQ -> return (select (y,x), xs)
         LT -> mzero

singleton :: k -> a -> T k a
singleton k a = Cons (k,a) Map.empty

member :: (Ord k) => k -> T k a -> Bool
member y (Cons x xs) =
   y == fst x || Map.member y xs

size :: T k a -> Int
size (Cons _ xs) = 1 + Map.size xs

elems :: T k a -> NonEmpty.T [] a
elems (Cons x xs) = NonEmpty.cons (snd x) (Map.elems xs)

keys :: T k a -> NonEmpty.T [] k
keys (Cons x xs) = NonEmpty.cons (fst x) (Map.keys xs)

-- 'insert' could be optimized to 'Cons'
keysSet :: (Ord k) => T k a -> NonEmptySet.T k
keysSet (Cons x xs) = NonEmptySet.insert (fst x) (Map.keysSet xs)

lookup :: (Ord k) => k -> T k a -> Maybe a
lookup y (Cons x xs) =
   if y == fst x
     then Just $ snd x
     else Map.lookup y xs

-- | prop> \k -> forAllNonEmptyMap $ \m -> Map.delete k (NonEmptyMap.flatten m) == NonEmptyMap.delete k m
delete :: (Ord k) => k -> T k a -> Map k a
delete y (Cons x xs) =
   if y == fst x then xs else uncurry Map.insert x $ Map.delete y xs

minViewWithKey :: T k a -> ((k,a), Map k a)
minViewWithKey (Cons x xs) = (x,xs)

maxViewWithKey :: (Ord k) => T k a -> ((k,a), Map k a)
maxViewWithKey (Cons x xs) =
   forcePair $
   case Map.maxViewWithKey xs of
      Nothing -> (x,xs)
      Just (y,ys) -> (y, uncurry Map.insert x ys)

{-# WARNING fromList "Dangerous because it silently drops colliding key/value pairs. Better use fromListWith." #-}
-- | prop> \xs -> Map.fromList (NonEmpty.flatten xs) == NonEmptyMap.flatten (NonEmptyMap.fromList (xs::NonEmpty.T [] (Int,Char)))
fromList :: (Ord k) => NonEmpty.T [] (k,a) -> T k a
fromList (NonEmpty.Cons x xs) = insertRight x $ Map.fromList xs

-- | prop> \xs -> Map.fromListWith (++) (NonEmpty.flatten xs) == NonEmptyMap.flatten (NonEmptyMap.fromListWith (++) (xs::NonEmpty.T [] (Int,String)))
fromListWith :: (Ord k) => (a -> a -> a) -> NonEmpty.T [] (k,a) -> T k a
fromListWith f (NonEmpty.Cons x xs) =
   insertRightWith f x $ Map.fromListWith f xs

-- | prop> forAllNonEmptyMap $ \m -> NonEmptyMap.fromAscList (NonEmptyMap.toAscList m) == m
fromAscList :: (Ord k) => NonEmpty.T [] (k,a) -> T k a
fromAscList (NonEmpty.Cons x xs) = Cons x $ Map.fromAscList xs

-- | prop> forAllNonEmptyMap $ \m -> NonEmpty.flatten (NonEmptyMap.toAscList m) == Map.toAscList (NonEmptyMap.flatten m)
toAscList :: T k a -> NonEmpty.T [] (k,a)
toAscList (Cons x xs) = NonEmpty.cons x $ Map.toAscList xs

fetch :: (Ord k) => Map k a -> Maybe (T k a)
fetch  =  fmap (uncurry Cons) . Map.minViewWithKey

flatten :: (Ord k) => T k a -> Map k a
flatten (Cons x xs) = uncurry Map.insert x xs


{-# WARNING union "Dangerous because it silently drops colliding key/value pairs. Better use unionWith." #-}
{-
Could be implemented in terms of unionRight
but that would require inspection of the plain Map using Map.minViewWithKey.
-}
-- | prop> forAllNonEmptyMap $ \xs -> forAllNonEmptyMap $ \ys -> Map.union (NonEmptyMap.flatten xs) (NonEmptyMap.flatten ys) == NonEmptyMap.flatten (NonEmptyMap.union xs ys)
union :: (Ord k) => T k a -> T k a -> T k a
union (Cons x xs) (Cons y ys) =
   uncurry Cons $
   case Map.union xs ys of
      zs ->
         case comparing fst x y of
            LT -> (x, uncurry (Map.insertWith (flip const)) y zs)
            GT -> (y, uncurry Map.insert x zs)
            EQ -> (x, zs)

{-# WARNING unionLeft "Dangerous because it silently drops colliding key/value pairs. Better use unionLeftWith." #-}
-- | prop> forAllMap $ \xm -> forAllNonEmptyMap $ \ym -> Map.union xm (NonEmptyMap.flatten ym) == NonEmptyMap.flatten (NonEmptyMap.unionLeft xm ym)
unionLeft :: (Ord k) => Map k a -> T k a -> T k a
unionLeft xs (Cons y ys) = insertRight y $ Map.union xs ys

{-# WARNING unionRight "Dangerous because it silently drops colliding key/value pairs. Better use unionRightWith." #-}
-- | prop> forAllNonEmptyMap $ \xm -> forAllMap $ \ym -> Map.union (NonEmptyMap.flatten xm) ym == NonEmptyMap.flatten (NonEmptyMap.unionRight xm ym)
unionRight :: (Ord k) => T k a -> Map k a -> T k a
unionRight (Cons x xs) ys = uncurry insert x $ Map.union xs ys


-- | prop> forAllNonEmptyMap $ \xs -> forAllNonEmptyMap $ \ys -> Map.unionWith (++) (NonEmptyMap.flatten xs) (NonEmptyMap.flatten ys) == NonEmptyMap.flatten (NonEmptyMap.unionWith (++) xs ys)
unionWith :: (Ord k) => (a -> a -> a) -> T k a -> T k a -> T k a
unionWith f (Cons x xs) (Cons y ys) =
   uncurry Cons $
   case Map.unionWith f xs ys of
      zs ->
         case comparing fst x y of
            LT -> (x, uncurry (Map.insertWith (flip f)) y zs)
            GT -> (y, uncurry (Map.insertWith f) x zs)
            EQ -> (applyFst f (x,y), zs)

-- | prop> forAllMap $ \xm -> forAllNonEmptyMap $ \ym -> Map.unionWith (++) xm (NonEmptyMap.flatten ym) == NonEmptyMap.flatten (NonEmptyMap.unionLeftWith (++) xm ym)
unionLeftWith :: (Ord k) => (a -> a -> a) -> Map k a -> T k a -> T k a
unionLeftWith f xs (Cons y ys) =
   insertRightWith f y $ Map.unionWith f xs ys

-- | prop> forAllNonEmptyMap $ \xm -> forAllMap $ \ym -> Map.unionWith (++) (NonEmptyMap.flatten xm) ym == NonEmptyMap.flatten (NonEmptyMap.unionRightWith (++) xm ym)
unionRightWith :: (Ord k) => (a -> a -> a) -> T k a -> Map k a -> T k a
unionRightWith f (Cons x xs) ys =
   uncurry (insertWith f) x $ Map.unionWith f xs ys


map :: (Ord k) => (a -> b) -> T k a -> T k b
map f (Cons x xs) = Cons (mapSnd f x) (Map.map f xs)

mapWithKey :: (Ord k) => (k -> a -> b) -> T k a -> T k b
mapWithKey f (Cons x@(k,_a) xs) = Cons (k, uncurry f x) (Map.mapWithKey f xs)
