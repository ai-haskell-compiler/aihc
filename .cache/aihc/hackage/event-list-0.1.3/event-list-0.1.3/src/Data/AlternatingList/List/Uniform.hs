{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

Lists of elements of alternating type.
This module is based on the standard list type
and may benefit from list optimizations.
-}
module Data.AlternatingList.List.Uniform
   (T(Cons),
    map, mapFirst, mapSecond,
    zipWithFirst, zipWithSecond,
    concatMonoid, concatMapMonoid,
    sequence, sequence_,
    traverse, traverse_, traverseFirst, traverseSecond,
    getFirsts, getSeconds, length, genericLength,
    fromFirstList, fromSecondList, fromEitherList,
    singleton, isSingleton,
    cons, snoc, reverse,
    mapSecondHead, forceSecondHead,
    foldr, foldl,
    format,
    filterFirst, partitionFirst, partitionMaybeFirst,
    partitionEitherFirst, unzipEitherFirst,
    filterSecond, partitionSecond, partitionMaybeSecond,
    partitionEitherSecond, unzipEitherSecond,

    catMaybesFirst, catMaybesSecond,
   ) where

import qualified Data.AlternatingList.List.Disparate as Disp

import qualified Control.Monad as Monad
import qualified Control.Applicative as Applicative
import qualified Data.List as List

import Control.Applicative (Applicative, pure, )
import Data.Monoid (Monoid, mempty, mappend, )

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

import Text.Show (Show, ShowS, showsPrec, showParen, showString, )
import Data.Function (id, flip, (.), ($), )
import Data.Tuple.HT (mapFst, mapSnd, mapPair, )
import Data.Maybe.HT (toMaybe, )
import Data.Functor (fmap, )
import Data.Either (Either(Left, Right), either, )
import Data.Maybe (Maybe, maybe, )
import Data.Tuple (uncurry, )
import Data.Ord (Ord, (>=), )
{- this way we cannot access (:) in Hugs -}
import Prelude (Integral, Int, String, Bool, error, Eq, )


{- |
The constructor is only exported for use in "Data.AlternatingList.List.Mixed".
-}
data T a b = Cons {
   _lead :: b,
   disp  :: Disp.T a b
   }
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
             Arbitrary (T a b) where
   arbitrary = Monad.liftM2 Cons arbitrary arbitrary
   shrink (Cons x xs) = fmap (uncurry Cons) $ shrink (x,xs)



map :: (a0 -> a1) -> (b0 -> b1) -> T a0 b0 -> T a1 b1
map f g (Cons b xs) = Cons (g b) (Disp.map f g xs)

mapFirst :: (a0 -> a1) -> T a0 b -> T a1 b
mapFirst f (Cons b xs) = Cons b (Disp.mapFirst f xs)

mapSecond :: (b0 -> b1) -> T a b0 -> T a b1
mapSecond g (Cons b xs) = Cons (g b) (Disp.mapSecond g xs)


zipWithFirst :: (a0 -> a1 -> a2) -> [a0] -> T a1 b -> T a2 b
zipWithFirst f xs (Cons a bs) =
   Cons a $ Disp.zipWithFirst f xs bs

zipWithSecond :: (b0 -> b1 -> b2) -> (b0,[b0]) -> T a b1 -> T a b2
zipWithSecond f (x,xs) (Cons a bs) =
   Cons (f x a) $ Disp.zipWithSecond f xs bs



concatMonoid :: Monoid m =>
   T m m -> m
concatMonoid =
   foldr mappend mappend mempty

concatMapMonoid :: Monoid m =>
   (time -> m) ->
   (body -> m) ->
   T time body -> m
concatMapMonoid f g =
   concatMonoid . map f g


sequence :: Applicative m =>
   T (m a) (m b) -> m (T a b)
sequence (Cons b xs) =
   Applicative.liftA2 Cons b (Disp.sequence xs)

sequence_ :: (Applicative m, Monoid d) =>
   T (m d) (m d) -> m d
sequence_ (Cons b xs) =
   Applicative.liftA2 mappend b $ Disp.sequence_ xs


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
getFirsts = Disp.getFirsts . disp

getSeconds :: T a b -> [b]
getSeconds (Cons b xs) = b : Disp.getSeconds xs

length :: T a b -> Int
length = List.length . getFirsts

genericLength :: Integral i => T a b -> i
genericLength = List.genericLength . getFirsts


fromFirstList :: b -> [a] -> T a b
fromFirstList b as =
   Cons b (List.foldr (flip Disp.cons b) Disp.empty as)

fromSecondList :: a -> [b] -> T a b
fromSecondList a (b:bs) =
   Cons b (List.foldr (Disp.cons a) Disp.empty bs)
fromSecondList _ [] = error "fromSecondList: empty list"

fromEitherList :: [Either a b] -> T a [b]
fromEitherList =
   List.foldr
      (either
         (cons [])
         (mapSecondHead . (:)))
      (singleton [])


singleton :: b -> T a b
singleton b = Cons b Disp.empty

isSingleton :: T a b -> Bool
isSingleton = Disp.null . disp


cons :: b -> a -> T a b -> T a b
cons b0 a ~(Cons b1 xs) = Cons b0 (Disp.cons a b1 xs)


snoc :: T a b -> a -> b -> T a b
snoc (Cons b0 xs) a b1 = Cons b0 (Disp.snoc xs a b1)


mapSecondHead :: (b -> b) -> T a b -> T a b
mapSecondHead f ~(Cons b xs) = Cons (f b) xs

forceSecondHead :: T a b -> T a b
forceSecondHead = mapSecondHead id



foldr :: (a -> c -> d) -> (b -> d -> c) -> d -> T a b -> c
foldr f g d (Cons b xs) = g b $ Disp.foldr f g d xs
{-
The lazy pattern match leads to a space leak in synthesizer-alsa:testArrangeSpaceLeak
I would like to reproduce this in a small test,
but I did not achieve this so far.
-}
-- foldr f g d ~(Cons b xs) = g b $ Disp.foldr f g d xs

foldl :: (c -> a -> d) -> (d -> b -> c) -> d -> T a b -> c
foldl f g d0 xs =
   foldr (\a go c -> go (f c a)) (\b go d -> go (g d b)) id xs d0

-- for a nicer implementation see Mixed
reverse :: T a b -> T a b
reverse =
   foldl (\ ~(Cons a xs) b -> Disp.cons b a xs) (flip Cons) Disp.empty


filterFirst :: (a -> Bool) -> T a b -> T a [b]
filterFirst p =
   catMaybesFirst . mapFirst (\a -> toMaybe (p a) a)

filterSecond :: (b -> Bool) -> T a b -> T b [a]
filterSecond p =
   catMaybesSecond . mapSecond (\a -> toMaybe (p a) a)

partitionFirst :: (a -> Bool) -> T a b -> (T a [b], T a [b])
partitionFirst p =
   unzipEitherFirst .
   mapFirst (\a -> if p a then Left a else Right a)

partitionSecond :: (b -> Bool) -> T a b -> (T b [a], T b [a])
partitionSecond p =
   unzipEitherSecond .
   mapSecond (\b -> if p b then Left b else Right b)

partitionMaybeFirst :: (a0 -> Maybe a1) -> T a0 b -> (T a1 [b], T a0 [b])
partitionMaybeFirst f =
   unzipEitherFirst . mapFirst (\a0 -> maybe (Right a0) Left (f a0))

partitionMaybeSecond :: (b0 -> Maybe b1) -> T a b0 -> (T b1 [a], T b0 [a])
partitionMaybeSecond f =
   unzipEitherSecond . mapSecond (\b0 -> maybe (Right b0) Left (f b0))

partitionEitherFirst :: (a -> Either a0 a1) -> T a b -> (T a0 [b], T a1 [b])
partitionEitherFirst f =
   unzipEitherFirst . mapFirst f

partitionEitherSecond :: (b -> Either b0 b1) -> T a b -> (T b0 [a], T b1 [a])
partitionEitherSecond f =
   unzipEitherSecond . mapSecond f

unzipEitherFirst :: T (Either a0 a1) b -> (T a0 [b], T a1 [b])
unzipEitherFirst =
   foldr
      (either
          (mapFst . cons [])
          (mapSnd . cons []))
      (\b -> mapPair (mapSecondHead (b:), mapSecondHead (b:)))
      (singleton [], singleton [])

unzipEitherSecond :: T a (Either b0 b1) -> (T b0 [a], T b1 [a])
unzipEitherSecond =
   foldr
      (\a -> mapPair (mapSecondHead (a:), mapSecondHead (a:)))
      (either
          (mapFst . cons [])
          (mapSnd . cons []))
      (singleton [], singleton [])

catMaybesFirst :: T (Maybe a) b -> T a [b]
catMaybesFirst =
   foldr
      (maybe id (cons []))
      (mapSecondHead . (:))
      (singleton [])

catMaybesSecond :: T a (Maybe b) -> T b [a]
catMaybesSecond =
   foldr
      (mapSecondHead . (:))
      (maybe id (cons []))
      (singleton [])
