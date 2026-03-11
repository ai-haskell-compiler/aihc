{- |
Copyright   :  (c) Henning Thielemann 2007-2009

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

Lists of elements of alternating type.
This module is based on the standard list type
and may benefit from list optimizations.
-}
module Data.AlternatingList.List.Disparate
   (T,
    fromPairList, toPairList,
    map, mapFirst, mapSecond,
    zipWithFirst, zipWithSecond,
    concatMonoid, concatMapMonoid,
    sequence, sequence_,
    traverse, traverse_, traverseFirst, traverseSecond,
    getFirsts, getSeconds, length, genericLength,
    empty, singleton, null,
    cons, snoc, viewL, viewR, switchL, switchR, mapHead, mapLast,
    foldr, foldrPair, foldl, reverse,
    format,
    append, concat, cycle,
    splitAt, take, drop,
    genericSplitAt, genericTake, genericDrop,
    spanFirst, spanSecond,
   ) where

import Data.Tuple.HT (mapSnd, mapPair, )

import qualified Data.List as List
import qualified Data.List.HT as ListHT
import qualified Control.Monad as Monad
import qualified Control.Applicative as Applicative
import qualified Data.Traversable as Trav

import Control.Applicative (Applicative, pure, )
import Data.Monoid (Monoid, mempty, mappend, )

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

import Text.Show (Show, ShowS, showsPrec, showParen, showString, )
import Data.Function (id, flip, (.), ($), )
import Data.Functor (fmap, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.List (zipWith, (++), )
import Data.Tuple (curry, uncurry, )
import Data.Ord (Ord, (>=), )
import Prelude (Integral, Int, String, Bool, Eq, )


data Pair a b =
     Pair {pairFirst  :: a,
           pairSecond :: b}
   deriving (Eq, Ord, Show)

newtype T a b = Cons {decons :: [Pair a b]}
   deriving (Eq, Ord)


format :: (Show a, Show b) =>
   String -> String -> Int -> T a b -> ShowS
format first second p xs =
   showParen (p>=5) $
   flip (foldr
      (\a -> showsPrec 5 a . showString first)
      (\b -> showsPrec 5 b . showString second))
      xs .
      showString "empty"

instance (Show a, Show b) => Show (T a b) where
   showsPrec = format " /. " " ./ "


instance (Arbitrary a, Arbitrary b) =>
             Arbitrary (Pair a b) where
   arbitrary = Monad.liftM2 Pair arbitrary arbitrary
   shrink (Pair a b) = fmap (uncurry Pair) $ shrink (a,b)

instance (Arbitrary a, Arbitrary b) =>
             Arbitrary (T a b) where
   arbitrary = Monad.liftM Cons arbitrary
   shrink (Cons xs) = fmap Cons $ shrink xs


fromPairList :: [(a,b)] -> T a b
fromPairList = Cons . List.map (uncurry Pair)

toPairList :: T a b -> [(a,b)]
toPairList = List.map (\ ~(Pair a b) -> (a,b)) . decons


lift :: ([Pair a0 b0] -> [Pair a1 b1]) -> (T a0 b0 -> T a1 b1)
lift f = Cons . f . decons

{-# INLINE mapPairFirst #-}
mapPairFirst :: (a0 -> a1) -> Pair a0 b -> Pair a1 b
mapPairFirst f e = e{pairFirst = f (pairFirst e)}

{-# INLINE mapPairSecond #-}
mapPairSecond :: (b0 -> b1) -> Pair a b0 -> Pair a b1
mapPairSecond f e = e{pairSecond = f (pairSecond e)}

{-# INLINE map #-}
map :: (a0 -> a1) -> (b0 -> b1) -> T a0 b0 -> T a1 b1
map f g = lift (List.map (mapPairFirst f . mapPairSecond g))

{-# INLINE mapFirst #-}
mapFirst :: (a0 -> a1) -> T a0 b -> T a1 b
mapFirst f = lift (List.map (mapPairFirst f))

{-# INLINE mapSecond #-}
mapSecond :: (b0 -> b1) -> T a b0 -> T a b1
mapSecond g = lift (List.map (mapPairSecond g))


zipWithFirst :: (a0 -> a1 -> a2) -> [a0] -> T a1 b -> T a2 b
zipWithFirst f xs =
   lift $ zipWith (\x (Pair a b) -> Pair (f x a) b) xs

zipWithSecond :: (b0 -> b1 -> b2) -> [b0] -> T a b1 -> T a b2
zipWithSecond f xs =
   lift $ zipWith (\x (Pair a b) -> Pair a (f x b)) xs


{- |
Counterpart to 'Foldable.fold'.
-}
concatMonoid :: Monoid m =>
   T m m -> m
concatMonoid =
   foldr mappend mappend mempty

{- |
Counterpart to 'Foldable.foldMap'.
-}
concatMapMonoid :: Monoid m =>
   (time -> m) ->
   (body -> m) ->
   T time body -> m
concatMapMonoid f g =
   concatMonoid . map f g


sequence :: Applicative m =>
   T (m a) (m b) -> m (T a b)
sequence =
   Applicative.liftA Cons .
   Trav.traverse (\(Pair a b) -> Applicative.liftA2 Pair a b) .
   decons

sequence_ :: (Applicative m, Monoid d) =>
   T (m d) (m d) -> m d
sequence_ =
   foldr (Applicative.liftA2 mappend) (Applicative.liftA2 mappend) $ pure mempty
--   Trav.traverse_ (\(Pair a b) -> Applicative.liftA2 mappend a b) . decons


traverse :: Applicative m =>
   (a0 -> m a1) -> (b0 -> m b1) ->
   T a0 b0 -> m (T a1 b1)
traverse aAction bAction =
   sequence . map aAction bAction

traverse_ :: (Applicative m, Monoid d) =>
   (a -> m d) -> (b -> m d) -> T a b -> m d
traverse_ aAction bAction =
   sequence_ . map aAction bAction


traverseFirst :: Applicative m =>
   (a0 -> m a1) -> T a0 b -> m (T a1 b)
traverseFirst aAction =
   traverse aAction pure

traverseSecond :: Applicative m =>
   (b0 -> m b1) -> T a b0 -> m (T a b1)
traverseSecond bAction =
   traverse pure bAction


getFirsts :: T a b -> [a]
getFirsts = List.map pairFirst . decons

getSeconds :: T a b -> [b]
getSeconds = List.map pairSecond . decons

length :: T a b -> Int
length = List.length . getFirsts

genericLength :: Integral i => T a b -> i
genericLength = List.genericLength . getFirsts



empty :: T a b
empty = Cons []

singleton :: a -> b -> T a b
singleton a b = Cons [Pair a b]

null :: T a b -> Bool
null = List.null . decons


cons :: a -> b -> T a b -> T a b
cons a b = lift (Pair a b : )

snoc :: T a b -> a -> b -> T a b
snoc (Cons xs) a b = Cons (xs ++ [Pair a b])


viewL :: T a b -> Maybe ((a, b), T a b)
viewL =
   switchL Nothing (\a b xs -> Just ((a, b), xs))

{-# INLINE switchL #-}
switchL :: c -> (a -> b -> T a b -> c) -> T a b -> c
switchL f g (Cons ys) =
   case ys of
      (Pair a b : xs) -> g a b (Cons xs)
      [] -> f

{-# INLINE mapHead #-}
mapHead :: ((a,b) -> (a,b)) -> T a b -> T a b
mapHead f =
   switchL empty (curry (uncurry cons . f))
--   maybe empty (uncurry (uncurry cons) . mapFst f) . viewL


viewR :: T a b -> Maybe (T a b, (a, b))
viewR =
   fmap (mapPair (Cons, \ ~(Pair a b) -> (a, b))) .
   ListHT.viewR . decons

{-# INLINE switchR #-}
switchR :: c -> (T a b -> a -> b -> c) -> T a b -> c
switchR f g =
   maybe f (\ ~(xs, ~(Pair a b)) -> g (Cons xs) a b) .
   ListHT.viewR . decons

{-# INLINE mapLast #-}
mapLast :: ((a,b) -> (a,b)) -> T a b -> T a b
mapLast f =
   maybe empty (uncurry (uncurry . snoc) . mapSnd f) . viewR


foldr :: (a -> c -> d) -> (b -> d -> c) -> d -> T a b -> d
foldr f g =
   foldrPair (\ a b -> f a . g b)

foldrPair :: (a -> b -> c -> c) -> c -> T a b -> c
foldrPair f x =
   List.foldr (\ ~(Pair a b) -> f a b) x . decons

foldl :: (c -> a -> d) -> (d -> b -> c) -> c -> T a b -> c
foldl f g c0 xs =
   foldr (\a go c -> go (f c a)) (\b go d -> go (g d b)) id xs c0


append :: T a b -> T a b -> T a b
append (Cons xs) = lift (xs++)

concat :: [T a b] -> T a b
concat = Cons . List.concat . List.map decons

cycle :: T a b -> T a b
cycle = Cons . List.cycle . decons

-- for a nicer implementation see Mixed
reverse :: T a b -> T b a
reverse =
   foldl (flip (,)) (\ ~(a,xs) b -> cons b a xs) empty



{- |
Currently it is not checked, whether n is too big.
Don't rely on the current behaviour of @splitAt n x@ for @n > length x@.
-}
splitAt :: Int -> T a b -> (T a b, T a b)
splitAt n = mapPair (Cons, Cons) . List.splitAt n . decons

take :: Int -> T a b -> T a b
take n = Cons . List.take n . decons

drop :: Int -> T a b -> T a b
drop n = Cons . List.drop n . decons


genericSplitAt :: Integral i => i -> T a b -> (T a b, T a b)
genericSplitAt n = mapPair (Cons, Cons) . List.genericSplitAt n . decons

genericTake :: Integral i => i -> T a b -> T a b
genericTake n = Cons . List.genericTake n . decons

genericDrop :: Integral i => i -> T a b -> T a b
genericDrop n = Cons . List.genericDrop n . decons


spanFirst :: (a -> Bool) -> T a b -> (T a b, T a b)
spanFirst p =
   mapPair (Cons, Cons) . List.span (p . pairFirst) . decons

spanSecond :: (b -> Bool) -> T a b -> (T a b, T a b)
spanSecond p =
   mapPair (Cons, Cons) . List.span (p . pairSecond) . decons

{-
filterFirst :: (a -> Bool) -> T a b -> T a [b]
filterFirst =
   foldr
      (\time ->
          if time==0
            then id
            else consBody [] . consTime time)
      (\body ->
          maybe
             (consBody [body] $ consTime 0 $ empty)
             (\(bodys,xs) -> consBody (body:bodys) xs) .
          viewBodyL)
      empty
-}
