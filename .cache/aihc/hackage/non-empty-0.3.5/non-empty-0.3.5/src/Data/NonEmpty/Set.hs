module Data.NonEmpty.Set (
   T,
   insert,
   singleton,
   member,
   size,
   fromList,
   fromAscList,
   toAscList,
   fetch,
   flatten,
   union,
   unionLeft,
   unionRight,

   findMin,
   findMax,
   delete,
   deleteMin,
   deleteMax,
   deleteFindMin,
   deleteFindMax,
   minView,
   maxView,
   ) where

import qualified Data.NonEmpty.Class as C
import qualified Data.NonEmpty as NonEmpty

import qualified Data.Set as Set
import Data.Set (Set, )

import Control.Monad (mzero, )
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData, rnf, )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple.HT (forcePair, mapSnd, )

import qualified Test.QuickCheck as QC


{-
The first field will always contain the smallest element.
We do not use the NonEmpty data type here
since it is easy to break this invariant using NonEmpty.!:.
The custom type is also consistent with Map.
-}
data T a = Cons a (Set a)
   deriving (Eq, Ord)

instance (Show a) => Show (T a) where
   showsPrec p xs =
      showParen (p>10) $
         showString "NonEmptySet.fromList " .
         showsPrec 11 (toAscList xs)


instance (NFData a) => NFData (T a) where
   rnf = C.rnf

instance C.NFData T where
   rnf (Cons x xs) = rnf (x, C.rnf xs)


instance (QC.Arbitrary a, Ord a) => QC.Arbitrary (T a) where
   arbitrary = liftA2 insert QC.arbitrary QC.arbitrary
   shrink = mapMaybe fetch . QC.shrink . flatten


{- |
We cannot have a reasonable @instance Insert Set@,
since the @instance Insert (NonEmpty Set)@
would preserve duplicate leading elements, whereas 'Set' does not.

However, the @instance Insert NonEmpty@ is not the problem.
A general type like

> insertSet :: (Insert f, Ord a) => a -> f a -> NonEmpty f a

cannot work, since it can be instantiated to

> insertSet :: (Ord a) => a -> NonEmpty Set a -> NonEmpty (NonEmpty Set) a

and this is obviously wrong:
@insertSet x (singleton x)@ has only one element, not two.
-}
insert :: Ord a => a -> Set a -> T a
insert = insertGen fst

insertGen :: Ord a => ((a,a) -> a) -> a -> Set a -> T a
insertGen select y xt =
   uncurry Cons $
   fromMaybe (y, xt) $ do
      (x,xs) <- Set.minView xt
      case compare y x of
         GT -> return (x, Set.insert y xs)
         EQ -> return (select (y,x), xs)
         LT -> mzero

singleton :: a -> T a
singleton a = Cons a Set.empty

member :: (Ord a) => a -> T a -> Bool
member y (Cons x xs) =
   y==x || Set.member y xs

size :: T a -> Int
size (Cons _ xs) = 1 + Set.size xs


findMin :: T a -> a
findMin (Cons x _) = x

findMax :: T a -> a
findMax (Cons x xs) =
   if Set.null xs then x else Set.findMax xs


delete :: (Ord k) => k -> T k -> Set k
delete y (Cons x xs) =
   if y == x then xs else Set.insert x $ Set.delete y xs

deleteMin :: T a -> Set a
deleteMin (Cons _ xs) = xs

deleteMax :: (Ord a) => T a -> Set a
deleteMax (Cons x xs) =
   if Set.null xs then Set.empty else Set.insert x $ Set.deleteMax xs


deleteFindMin :: T a -> (a, Set a)
deleteFindMin (Cons x xs) = (x, xs)

deleteFindMax :: (Ord a) => T a -> (a, Set a)
deleteFindMax (Cons x xs) =
   if Set.null xs
     then (x, Set.empty)
     else mapSnd (Set.insert x) $ Set.deleteFindMax xs


minView :: T a -> (a, Set a)
minView (Cons x xs) = (x,xs)

maxView :: (Ord a) => T a -> (a, Set a)
maxView (Cons x xs) =
   forcePair $
   case Set.maxView xs of
      Nothing -> (x,xs)
      Just (y,ys) -> (y, Set.insert x ys)


fromList :: (Ord a) => NonEmpty.T [] a -> T a
fromList (NonEmpty.Cons x xs) = insert x $ Set.fromList xs

fromAscList :: (Ord a) => NonEmpty.T [] a -> T a
fromAscList (NonEmpty.Cons x xs) = Cons x $ Set.fromAscList xs

toAscList :: T a -> NonEmpty.T [] a
toAscList (Cons x xs) = NonEmpty.Cons x $ Set.toAscList xs

fetch :: (Ord a) => Set a -> Maybe (T a)
fetch  =  fmap (uncurry Cons) . Set.minView

flatten :: (Ord a) => T a -> Set a
flatten (Cons x xs) = Set.insert x xs

union :: (Ord a) => T a -> T a -> T a
union (Cons x xs) (Cons y ys) =
   uncurry Cons $
   case Set.union xs ys of
      zs ->
         case compare x y of
            LT -> (x, Set.union zs $ Set.singleton y)
            GT -> (y, Set.insert x zs)
            EQ -> (x, zs)

unionLeft :: (Ord a) => Set a -> T a -> T a
unionLeft xs (Cons y ys) =
   insertGen snd y $ Set.union xs ys

unionRight :: (Ord a) => T a -> Set a -> T a
unionRight (Cons x xs) ys =
   insertGen fst x $ Set.union xs ys


{-
According Set functions are only available since containers-0.5.2, i.e. GHC-7.8.

elemAt :: Int -> T a -> a
elemAt k (Cons x xs) =
   if k==0 then x else Set.elemAt (pred k) xs

deleteAt :: Int -> T a -> Set a
deleteAt k (Cons _ xs) =
   if k==0 then xs else Set.deleteAt (pred k) xs
-}
