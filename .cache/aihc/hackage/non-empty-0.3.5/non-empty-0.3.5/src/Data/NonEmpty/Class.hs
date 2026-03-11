module Data.NonEmpty.Class where

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Ix as Ix
import qualified Data.List.Key as Key
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Control.DeepSeq as DeepSeq
import Data.Sequence (Seq, )
import Data.Map (Map, )
import Data.Set (Set, )
import Data.Traversable (Traversable, mapAccumL, mapAccumR)
import Control.Monad (liftM2, )
import Data.Tuple.HT (swap, )
import Data.Ord.HT (comparing, )

import qualified Test.QuickCheck as QC

import qualified Prelude as P
import Prelude hiding (Show, showsPrec, zipWith, zipWith3, reverse, )


class Empty f where
   empty :: f a

instance Empty [] where
   empty = []

instance Empty Maybe where
   empty = Nothing

instance Empty Set where
   empty = Set.empty

instance Empty (Map k) where
   empty = Map.empty

instance Empty Seq where
   empty = Seq.empty


class Cons f where
   cons :: a -> f a -> f a

instance Cons [] where
   cons = (:)

instance Cons Seq where
   cons = (Seq.<|)


class Snoc f where
   snoc :: f a -> a -> f a

instance Snoc [] where
   snoc = snocDefault

instance Snoc Seq where
   snoc = (Seq.|>)

snocDefault :: (Cons f, Traversable f) => f a -> a -> f a
snocDefault xs x =
   uncurry cons $ mapAccumR (flip (,)) x xs


class ViewL f where
   viewL :: f a -> Maybe (a, f a)

instance ViewL [] where
   viewL = ListHT.viewL

instance ViewL Maybe where
   viewL = fmap (\a -> (a, Nothing))

instance ViewL Set where
   viewL = Set.minView

instance ViewL Seq where
   viewL x =
      case Seq.viewl x of
         Seq.EmptyL -> Nothing
         y Seq.:< ys -> Just (y,ys)
   -- viewL x = do y Seq.:< ys <- Just $ Seq.viewl x; Just (y,ys)


class ViewR f where
   viewR :: f a -> Maybe (f a, a)

instance ViewR [] where
   viewR = ListHT.viewR

instance ViewR Maybe where
   viewR = fmap (\a -> (Nothing, a))

instance ViewR Set where
   viewR = fmap swap . Set.maxView

instance ViewR Seq where
   viewR x =
      case Seq.viewr x of
         Seq.EmptyR -> Nothing
         ys Seq.:> y -> Just (ys,y)


class (ViewL f, ViewR f) => View f where
instance View [] where
instance View Maybe where
instance View Set where
instance View Seq where


{-
Default implementation of 'viewR' based on 'viewL' and 'Traversable'.
-}
viewRDefault :: (ViewL f, Traversable f) => f a -> Maybe (f a, a)
viewRDefault =
   fmap (swap . uncurry (mapAccumL (flip (,)))) . viewL


class Singleton f where
   singleton :: a -> f a

instance Singleton [] where
   singleton x = [x]

instance Singleton Maybe where
   singleton x = Just x

instance Singleton Set where
   singleton = Set.singleton

instance Singleton Seq where
   singleton = Seq.singleton


class Append f where
   append :: f a -> f a -> f a

instance Append [] where
   append = (++)

instance Append Seq where
   append = (Seq.><)

infixr 5 `cons`, `append`


{- |
It must hold:

> fmap f xs
>    = zipWith (\x _ -> f x) xs xs
>    = zipWith (\_ x -> f x) xs xs
-}
class Functor f => Zip f where
   zipWith :: (a -> b -> c) -> f a -> f b -> f c

instance Zip [] where
   zipWith = List.zipWith

instance Zip Maybe where
   zipWith = liftM2

instance Zip Seq where
   zipWith = Seq.zipWith

zipWith3 :: (Zip f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
zipWith3 f a b c = zipWith ($) (zipWith f a b) c

zipWith4 :: (Zip f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
zipWith4 f a b c d = zipWith ($) (zipWith3 f a b c) d

zip :: (Zip f) => f a -> f b -> f (a,b)
zip = zipWith (,)

zip3 :: (Zip f) => f a -> f b -> f c -> f (a,b,c)
zip3 = zipWith3 (,,)

zip4 :: (Zip f) => f a -> f b -> f c -> f d -> f (a,b,c,d)
zip4 = zipWith4 (,,,)


class Repeat f where
   {- |
   Create a container with as many copies as possible of a given value.
   That is, for a container with fixed size @n@,
   the call @repeat x@ will generate a container with @n@ copies of @x@.
   -}
   repeat :: a -> f a

instance Repeat [] where
   repeat = List.repeat

instance Repeat Maybe where
   repeat = Just


-- might be replaced by Mixed.iterate based on Traversable
class Repeat f => Iterate f where
   iterate :: (a -> a) -> a -> f a

instance Iterate [] where
   iterate = List.iterate

instance Iterate Maybe where
   iterate _ = Just


{- |
We need to distinguish between 'Sort' and 'SortBy',
since there is an @instance Sort Set@
but there cannot be an @instance SortBy Set@.
-}
class Sort f where
   sort :: (Ord a) => f a -> f a

instance Sort [] where
   sort = List.sort

instance Sort Maybe where
   sort = id

instance Sort Seq where
   sort = Seq.sort

instance Sort Set where
   sort = id

{- |
Default implementation for 'sort' based on 'sortBy'.
-}
sortDefault :: (Ord a, SortBy f) => f a -> f a
sortDefault = sortBy compare


class Sort f => SortBy f where
   sortBy :: (a -> a -> Ordering) -> f a -> f a

instance SortBy [] where
   sortBy = List.sortBy

instance SortBy Maybe where
   sortBy _f = id

instance SortBy Seq where
   sortBy = Seq.sortBy


class Sort f => SortKey f where
   sortKey :: (Ord b) => (a -> b) -> f a -> f a

instance SortKey [] where
   sortKey = Key.sort

instance SortKey Maybe where
   sortKey _f = id

instance SortKey Seq where
   sortKey = sortKeyGen

sortKeyGen :: (SortBy f, Functor f, Ord b) => (a -> b) -> f a -> f a
sortKeyGen f = fmap snd . sortBy (comparing fst) . fmap (\x -> (f x, x))


class Reverse f where
   reverse :: f a -> f a

instance Reverse [] where reverse = List.reverse
instance Reverse Maybe where reverse = id
instance Reverse Seq where reverse = Seq.reverse


class Show f where
   showsPrec :: P.Show a => Int -> f a -> ShowS

instance Show [] where
   showsPrec p xs =
      if null xs
        then showString "[]"
        else showParen (p>5) $
             foldr (.) (showString "[]") $
             map (\x -> P.showsPrec 6 x . showString ":") xs

instance Show Maybe where
   showsPrec = P.showsPrec

instance Show Seq where
   showsPrec = P.showsPrec

instance Show Set where
   showsPrec = P.showsPrec


class Arbitrary f where
   arbitrary :: QC.Arbitrary a => QC.Gen (f a)
   shrink :: QC.Arbitrary a => f a -> [f a]

instance Arbitrary [] where
   arbitrary = QC.arbitrary
   shrink = QC.shrink

instance Arbitrary Seq where
   arbitrary = QC.arbitrary
   shrink = QC.shrink

instance Arbitrary Maybe where
   arbitrary = QC.arbitrary
   shrink = QC.shrink

instance (QC.Arbitrary k, Ord k) => Arbitrary (Map k) where
   arbitrary = QC.arbitrary
   shrink = QC.shrink


class (Arbitrary f) => Gen f where
   genOf :: QC.Gen a -> QC.Gen (f a)

instance Gen [] where
   genOf = QC.listOf

instance Gen Seq where
   genOf = fmap Seq.fromList . QC.listOf

instance Gen Maybe where
   genOf gen = do
      b <- QC.arbitrary
      if b then fmap Just gen else return Nothing

instance (QC.Arbitrary k, Ord k) => Gen (Map k) where
   genOf gen =
      fmap Map.fromList $ mapM (\k -> fmap ((,) k) gen) =<< QC.arbitrary


class NFData f where
   rnf :: DeepSeq.NFData a => f a -> ()

instance NFData Maybe where
   rnf = DeepSeq.rnf

instance NFData [] where
   rnf = DeepSeq.rnf

instance NFData Set where
   rnf = DeepSeq.rnf

instance (DeepSeq.NFData k) => NFData (Map k) where
   rnf = DeepSeq.rnf


class Ix f where
   {-# MINIMAL range, (index | indexHorner), inRange #-}
   range :: (Ix.Ix i) => (f i, f i) -> [f i]
   index :: (Ix.Ix i) => (f i, f i) -> f i -> Int
   index =
      if True
         then flip indexHorner 0
         else snd . rangeSizeIndex
   inRange :: (Ix.Ix i) => (f i, f i) -> f i -> Bool
   rangeSize :: (Ix.Ix i) => (f i, f i) -> Int
   rangeSize b@(_l,h) = if inRange b h then index b h + 1 else 0
   {- |
   The default implementation causes quadratic runtime
   on nested index tuple types.
   This affects the 'index' function, too.
   -}
   rangeSizeIndex :: (Ix.Ix i) => (f i, f i) -> (Int, f i -> Int)
   rangeSizeIndex b = (rangeSize b, index b)
   {- |
   A custom implementation of this function
   allows for an even more efficient implementation
   of 'index' on nested NonEmpty constructors.
   -}
   indexHorner :: (Ix.Ix i) => (f i, f i) -> Int -> f i -> Int
   indexHorner b =
      let size = rangeSize b
      in \offset i -> offset * size + index b i
