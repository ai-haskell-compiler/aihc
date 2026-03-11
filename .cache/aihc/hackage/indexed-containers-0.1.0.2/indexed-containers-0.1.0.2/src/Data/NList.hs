{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.NList
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- Lists whose types are indexed by their lengths. The implementation is a simple
-- wrapper around a regular list.
--
-- All functions in this module are total. The time complexity of each function is
-- the same as that of the corresponding function on regular lists.
module Data.NList
  (
  -- * NList type
  NList

  -- * Basic functions

  , (<:>)
  , (<++>)
  , length
  , head
  , headMay
  , tail
  , tail'
  , tailMay
  , last
  , lastMay
  , init
  , init'
  , initMay
  , uncons
  , toList

  -- * Extracing sublists
  , take
  , drop
  , splitAt

  -- * Indexing
  , kth

  -- * Transformations
  , reverse
  , intersperse
  , transpose
  , concat

  -- * Ordered lists
  , sort
  , sortOn
  , sortBy

  -- * Zipping and unzipping
  , zip
  , zipWith
  , unzip

  -- * Construction
  , replicate
  , empty
  , singleton
  , mk1
  , mk2
  , mk3
  , mk4
  , mk5
  , mk6
  , mk7
  , mk8
  , mk9
  , mk10

  -- * To and from tuples
  , FromTuple(..)
  , ToTuple(..)

  -- * Predecessor of a Nat
  , Pred
  ) where

import qualified Data.List as List
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (-), type(*), type (<=))

import           Prelude hiding (concat, drop, head, init, last, length, replicate, reverse, splitAt, tail, take, unzip, zip, zipWith)

infixr 5 <:>

infixr 5 <++>

-- | A list whose length is statically known.
--
-- Type parameter @n@, of kind 'Nat', is the length of the list.
newtype NList (n :: Nat) a = List [a] deriving (Eq, Ord)

-- | The empty list.
--
-- > length empty === 0
empty :: NList 0 a
empty = List []

-- | A singleton list.
--
-- > length (singleton 'a' :: NList 1 Char) === 1
singleton :: a -> NList 1 a
singleton a = List [a]

-- | Prepend an element to a list.
--
-- > 'a' <:> singleton 'b' === mk2 'a' 'b'
(<:>) :: a -> NList n a -> NList (n + 1) a
(<:>) x (List xs) = List (x : xs)

-- | Append two lists.
--
-- > mk2 'a' 'b' <++> mk3 'c' 'd' 'e' === mk5 'a' 'b' 'c' 'd' 'e'
(<++>) :: NList n a -> NList m a -> NList (n + m) a
(<++>) (List xs) (List ys) = List (xs List.++ ys)

-- | Length of a list.
--
-- > length (mk3 'a' 'b' 'c') === 3
length :: NList n a -> Int
length (List xs) = List.length xs

-- | Head of a non-empty list.
--
-- > head (mk3 'a' 'b' 'c') === 'a'
head :: (1 <= n) => NList n a -> a
head (List xs) = List.head xs

-- | Head of a list.
--
-- > headMay (empty :: NList 0 Int) === Nothing
-- > headMay (mk3 'a' 'b' 'c') === Just 'a'
headMay :: NList n a -> Maybe a
headMay (List []) = Nothing
headMay (List (x:_)) = Just x

-- | Tail of a non-empty list.
--
-- > tail (singleton 'a') === empty
-- > tail (mk3 'a' 'b' 'c') === mk2 'b' 'c'
tail :: (1 <= n) => NList n a -> NList (Pred n) a
tail (List xs) = List (List.tail xs)

-- | Tail of a list. Returns an empty list if the input is empty.
--
-- > tail' (empty :: NList 0 ()) === empty
-- > tail' (singleton 'a') === empty
-- > tail' (mk3 'a' 'b' 'c') === mk2 'b' 'c'
tail' :: NList n a -> NList (Pred n) a
tail' (List []) = List []
tail' (List (_:xs)) = List xs

-- | Tail of a list. Returns Nothing if the input is empty.
--
-- > tailMay (empty :: NList 0 ()) === Nothing
-- > tailMay (singleton 'a') === Just empty
-- > tailMay (mk3 'a' 'b' 'c') === Just (mk2 'b' 'c')
tailMay :: NList n a -> Maybe (NList (Pred n) a)
tailMay (List []) = Nothing
tailMay (List (_:xs)) = Just (List xs)

-- | The last element of a non-empty list.
--
-- > last (mk3 'a' 'b' 'c') === 'c'
last :: (1 <= n) => NList n a -> a
last (List xs) = List.last xs

-- | The last element of a list.
--
-- > lastMay (empty :: NList 0 Int) === Nothing
-- > lastMay (mk3 'a' 'b' 'c') === Just 'c'
lastMay :: NList n a -> Maybe a
lastMay (List []) = Nothing
lastMay (List xs) = Just (List.last xs)

-- | All elements of a non-empty list except the last one.
--
-- > init (singleton 'a') === empty
-- > init (mk3 'a' 'b' 'c') === mk2 'a' 'b'
init :: (1 <= n) => NList n a -> NList (Pred n) a
init (List xs) = List (List.init xs)

-- | All elements of a list except the last one. Returns an empty list
-- if the input is empty.
--
-- > init' (empty :: NList 0 ()) === empty
-- > init' (singleton 'a') === empty
-- > init' (mk3 'a' 'b' 'c') === mk2 'a' 'b'
init' :: NList n a -> NList (Pred n) a
init' (List []) = List []
init' (List xs) = List (List.init xs)

-- | All elements of a list except the last one. Returns Nothing
-- if the input is empty.
--
-- > initMay (empty :: NList 0 ()) === Nothing
-- > initMay (singleton 'a') === Just empty
-- > initMay (mk3 'a' 'b' 'c') === Just (mk2 'a' 'b')
initMay :: NList n a -> Maybe (NList (Pred n) a)
initMay (List []) = Nothing
initMay (List xs) = Just $ List (List.init xs)

-- | Decompose a list into head and tail.
--
-- > uncons (singleton 'a') === ('a', empty)
-- > uncons (mk3 'a' 'b' 'c') === ('a', mk2 'b' 'c')
uncons :: (1 <= n) => NList n a -> (a, NList (n-1) a)
uncons (List (x:xs)) = (x, List xs)

-- | Return the first @k@ elements of a list whose length is at least @k@.
--
-- > take @0 (mk3 'a' 'b' 'c') === empty
-- > take @2 (mk3 'a' 'b' 'c') === mk2 'a' 'b'
-- > take @3 (mk3 'a' 'b' 'c') === mk3 'a' 'b' 'c'
take :: forall k n a. (KnownNat k, k <= n) => NList n a -> NList k a
take (List xs) = List (List.take k xs)
  where
    k = fromIntegral (natVal (Proxy :: Proxy k))

-- | Drop the first @k@ elements of a list whose length is at least @k@.
--
-- > drop @0 (mk3 'a' 'b' 'c') === mk3 'a' 'b' 'c'
-- > drop @2 (mk3 'a' 'b' 'c') === singleton 'c'
-- > drop @3 (mk3 'a' 'b' 'c') === empty
drop :: forall k n a. (KnownNat k, k <= n) => NList n a -> NList (n-k) a
drop (List xs) = List (List.drop k xs)
  where
    k = fromIntegral (natVal (Proxy :: Proxy k))

-- | Return the first @k@ elements, paired with the remaining elements, of
-- a list whose length is at least @k@.
--
-- > splitAt @0 (mk3 'a' 'b' 'c') === (empty, mk3 'a' 'b' 'c')
-- > splitAt @2 (mk3 'a' 'b' 'c') === (mk2 'a' 'b', singleton 'c')
-- > splitAt @3 (mk3 'a' 'b' 'c') === (mk3 'a' 'b' 'c', empty)
splitAt :: forall k n a. (KnownNat k, k <= n) => NList n a -> (NList k a, NList (n-k) a)
splitAt (List xs) = let (ys, zs) = List.splitAt k xs in (List ys, List zs)
  where
    k = fromIntegral (natVal (Proxy :: Proxy k))

-- | Reverse a list.
--
-- > reverse (mk3 'a' 'b' 'c') === mk3 'c' 'b' 'a'
reverse :: NList n a -> NList n a
reverse (List xs) = List (List.reverse xs)

-- | Take an element and a list, and insert the element in between elements
-- of the list.
--
-- > intersperse (',') empty === empty
-- > intersperse (',') (singleton 'a') === singleton 'a'
-- > intersperse (',') (mk3 'a' 'b' 'c') === mk5 'a' ',' 'b' ',' 'c'
intersperse :: a -> NList n a -> NList (Pred (n * 2)) a
intersperse x (List xs) = List (List.intersperse x xs)

-- | Transpose the rows and columns of a two dimensional list.
--
-- > transpose (mk2 (mk3 1 2 3) (mk3 4 5 6)) === mk3 (mk2 1 4) (mk2 2 5) (mk2 3 6)
transpose :: NList n (NList m a) -> NList m (NList n a)
transpose (List xss) = List . fmap List $ List.transpose (fmap toList xss)

-- | Return the element at index @k@ (starting from 0) in a list with at least
-- @k+1@ elements.
--
-- > kth @0 (mk4 'a' 'b' 'c' 'd') === 'a'
-- > kth @3 (mk4 'a' 'b' 'c' 'd') === 'd'
kth :: forall k n a. (KnownNat k, k <= n-1) => NList n a -> a
kth (List xs) = xs List.!! fromIntegral (natVal (Proxy :: Proxy k))

-- | Stably sort a list.
--
-- > sort (mk6 1 4 2 8 5 7) === mk6 1 2 4 5 7 8
sort :: Ord a => NList n a -> NList n a
sort (List xs) = List (List.sort xs)

-- | Sort a list by applying a function to each element and comparing the results.
--
-- > sortOn negate (mk6 1 4 2 8 5 7) === mk6 8 7 5 4 2 1
sortOn :: Ord b => (a -> b) -> NList n a -> NList n a
sortOn f (List xs) = List (List.sortOn f xs)

-- | Non-overloaded version of 'sort'.
--
-- > sortBy (\x y -> compare (-x) (-y)) (mk6 1 4 2 8 5 7) === mk6 8 7 5 4 2 1
sortBy :: (a -> a -> Ordering) -> NList n a -> NList n a
sortBy f (List xs) = List (List.sortBy f xs)

-- | Convert an 'NList' into a regular list.
--
-- > toList (mk3 'a' 'b' 'c') === "abc"
toList :: NList n a -> [a]
toList (List xs) = xs

-- | Zip two lists of the same length.
--
-- > zip (mk2 1 2) (mk2 'a' 'b') === mk2 (1, 'a') (2, 'b')
zip :: NList n a -> NList n b -> NList n (a, b)
zip (List xs) (List ys) = List (xs `List.zip` ys)

-- | Zip with a function.
--
-- > zipWith (+) (mk2 1 2) (mk2 3 4) === mk2 4 6
zipWith :: (a -> b -> c) -> NList n a -> NList n b -> NList n c
zipWith f (List xs) (List ys) = List (List.zipWith f xs ys)

-- | Unzip a list of pairs.
--
-- > unzip (mk2 (1, 'a') (2, 'b')) === ((mk2 1 2), (mk2 'a' 'b'))
unzip :: NList n (a, b) -> (NList n a, NList n b)
unzip (List xs) = (List ys, List zs)
  where
    (ys, zs) = List.unzip xs

-- | Concatenate the sublists of a two-dimensional list.
--
-- > concat (mk2 (mk3 1 2 3) (mk3 4 5 6)) === mk6 1 2 3 4 5 6
concat :: NList n (NList m a) -> NList (n * m) a
concat (List xss) = List (List.concatMap toList xss)

-- | Return a list containing @n@ copies of the given element.
--
-- > replicate @3 'a' === mk3 'a' 'a' 'a'
replicate ::forall n a. KnownNat n => a -> NList n a
replicate = List . List.replicate n
  where
    n = fromIntegral $ natVal (Proxy :: Proxy n)

-- |
-- > toList (mk1 'a') === "a"
mk1 :: a -> NList 1 a
mk1 = singleton

-- |
-- > toList (mk2 'a' 'b') === "ab"
mk2 :: a -> a -> NList 2 a
mk2 a1 a2 = List [a1, a2]

-- |
-- > toList (mk3 'a' 'b' 'c') === "abc"
mk3 :: a -> a -> a -> NList 3 a
mk3 a1 a2 a3 = List [a1, a2, a3]

-- |
-- > toList (mk4 'a' 'b' 'c' 'd') === "abcd"
mk4 :: a -> a -> a -> a -> NList 4 a
mk4 a1 a2 a3 a4 = List [a1, a2, a3, a4]

-- |
-- > toList (mk5 'a' 'b' 'c' 'd' 'e') === "abcde"
mk5 :: a -> a -> a -> a -> a -> NList 5 a
mk5 a1 a2 a3 a4 a5 = List [a1, a2, a3, a4, a5]

-- |
-- > toList (mk6 'a' 'b' 'c' 'd' 'e' 'f') === "abcdef"
mk6 :: a -> a -> a -> a -> a -> a -> NList 6 a
mk6 a1 a2 a3 a4 a5 a6 = List [a1, a2, a3, a4, a5, a6]

-- |
-- > toList (mk7 'a' 'b' 'c' 'd' 'e' 'f' 'g') === "abcdefg"
mk7 :: a -> a -> a -> a -> a -> a -> a -> NList 7 a
mk7 a1 a2 a3 a4 a5 a6 a7 = List [a1, a2, a3, a4, a5, a6, a7]

-- |
-- > toList (mk8 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h') === "abcdefgh"
mk8 :: a -> a -> a -> a -> a -> a -> a -> a -> NList 8 a
mk8 a1 a2 a3 a4 a5 a6 a7 a8 = List [a1, a2, a3, a4, a5, a6, a7, a8]

-- |
-- > toList (mk9 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i') === "abcdefghi"
mk9 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> NList 9 a
mk9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = List [a1, a2, a3, a4, a5, a6, a7, a8, a9]

-- |
-- > toList (mk10 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j') === "abcdefghij"
mk10 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> NList 10 a
mk10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = List [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]


-- | Typeclass for converting tuples to 'NList's.
class FromTuple a where
  type List a
  fromTuple :: a -> List a

instance FromTuple (a, a) where
  type List (a, a) = NList 2 a
  -- > fromTuple ('a', 'b') === mk2 'a' 'b'
  fromTuple (a1, a2) = mk2 a1 a2

instance FromTuple (a, a, a) where
  type List (a, a, a) = NList 3 a
  -- > fromTuple ('a', 'b', 'c') === mk3 'a' 'b' 'c'
  fromTuple (a1, a2, a3) = mk3 a1 a2 a3

instance FromTuple (a, a, a, a) where
  type List (a, a, a, a) = NList 4 a
  -- > fromTuple ('a', 'b', 'c', 'd') === mk4 'a' 'b' 'c' 'd'
  fromTuple (a1, a2, a3, a4) = mk4 a1 a2 a3 a4

instance FromTuple (a, a, a, a, a) where
  type List (a, a, a, a, a) = NList 5 a
  -- > fromTuple ('a', 'b', 'c', 'd', 'e') === mk5 'a' 'b' 'c' 'd' 'e'
  fromTuple (a1, a2, a3, a4, a5) = mk5 a1 a2 a3 a4 a5

instance FromTuple (a, a, a, a, a, a) where
  type List (a, a, a, a, a, a) = NList 6 a
  -- > fromTuple ('a', 'b', 'c', 'd', 'e', 'f') === mk6 'a' 'b' 'c' 'd' 'e' 'f'
  fromTuple (a1, a2, a3, a4, a5, a6) = mk6 a1 a2 a3 a4 a5 a6

instance FromTuple (a, a, a, a, a, a, a) where
  type List (a, a, a, a, a, a, a) = NList 7 a
  -- > fromTuple ('a', 'b', 'c', 'd', 'e', 'f', 'g') === mk7 'a' 'b' 'c' 'd' 'e' 'f' 'g'
  fromTuple (a1, a2, a3, a4, a5, a6, a7) = mk7 a1 a2 a3 a4 a5 a6 a7

instance FromTuple (a, a, a, a, a, a, a, a) where
  type List (a, a, a, a, a, a, a, a) = NList 8 a
  -- > fromTuple ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h') === mk8 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h'
  fromTuple (a1, a2, a3, a4, a5, a6, a7, a8) = mk8 a1 a2 a3 a4 a5 a6 a7 a8

instance FromTuple (a, a, a, a, a, a, a, a, a) where
  type List (a, a, a, a, a, a, a, a, a) = NList 9 a
  -- > fromTuple ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i') === mk9 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i'
  fromTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) = mk9 a1 a2 a3 a4 a5 a6 a7 a8 a9

instance FromTuple (a, a, a, a, a, a, a, a, a, a) where
  type List (a, a, a, a, a, a, a, a, a, a) = NList 10 a
  -- > fromTuple ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j') === mk10 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j'
  fromTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = mk10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10


-- | Typeclass for converting 'NList's to tuples.
class ToTuple a where
  type Tuple a
  toTuple :: a -> Tuple a

instance ToTuple (NList 2 a) where
  type Tuple (NList 2 a) = (a, a)
  -- > toTuple (mk2 'a' 'b') === ('a', 'b')
  toTuple (List [a1, a2]) = (a1, a2)

instance ToTuple (NList 3 a) where
  type Tuple (NList 3 a) = (a, a, a)
  -- > toTuple (mk3 'a' 'b' 'c') === ('a', 'b', 'c')
  toTuple (List [a1, a2, a3]) = (a1, a2, a3)

instance ToTuple (NList 4 a) where
  type Tuple (NList 4 a) = (a, a, a, a)
  -- > toTuple (mk4 'a' 'b' 'c' 'd') === ('a', 'b', 'c', 'd')
  toTuple (List [a1, a2, a3, a4]) = (a1, a2, a3, a4)

instance ToTuple (NList 5 a) where
  type Tuple (NList 5 a) = (a, a, a, a, a)
  -- > toTuple (mk5 'a' 'b' 'c' 'd' 'e') === ('a', 'b', 'c', 'd', 'e')
  toTuple (List [a1, a2, a3, a4, a5]) = (a1, a2, a3, a4, a5)

instance ToTuple (NList 6 a) where
  type Tuple (NList 6 a) = (a, a, a, a, a, a)
  -- > toTuple (mk6 'a' 'b' 'c' 'd' 'e' 'f') === ('a', 'b', 'c', 'd', 'e', 'f')
  toTuple (List [a1, a2, a3, a4, a5, a6]) = (a1, a2, a3, a4, a5, a6)

instance ToTuple (NList 7 a) where
  type Tuple (NList 7 a) = (a, a, a, a, a, a, a)
  -- > toTuple (mk7 'a' 'b' 'c' 'd' 'e' 'f' 'g') === ('a', 'b', 'c', 'd', 'e', 'f', 'g')
  toTuple (List [a1, a2, a3, a4, a5, a6, a7]) = (a1, a2, a3, a4, a5, a6, a7)

instance ToTuple (NList 8 a) where
  type Tuple (NList 8 a) = (a, a, a, a, a, a, a, a)
  -- > toTuple (mk8 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h') === ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
  toTuple (List [a1, a2, a3, a4, a5, a6, a7, a8]) = (a1, a2, a3, a4, a5, a6, a7, a8)

instance ToTuple (NList 9 a) where
  type Tuple (NList 9 a) = (a, a, a, a, a, a, a, a, a)
  -- > toTuple (mk9 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i') === ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
  toTuple (List [a1, a2, a3, a4, a5, a6, a7, a8, a9]) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance ToTuple (NList 10 a) where
  type Tuple (NList 10 a) = (a, a, a, a, a, a, a, a, a, a)
  -- > toTuple (mk10 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j') === ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
  toTuple (List [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance (KnownNat n, Show a) => Show (NList n a) where
  showsPrec p (List xs) = showParen (p > 10) $
    showString "List " . shows (natVal (Proxy :: Proxy n)) . showString " " . shows xs

instance Functor (NList n) where
  fmap f (List xs) = List (List.map f xs)

instance KnownNat n => Applicative (NList n) where
  pure = replicate
  (<*>) = zipWith ($)

instance Foldable (NList n) where
  foldr f z (List xs) = List.foldr f z xs

instance Traversable (NList n) where
  sequenceA (List xs) = List <$> sequenceA xs

-- | The 'Pred' type family is used to maintain the invariant that
-- @n@ is a 'KnownNat' (i.e., @n >= 0@) for all @List n a@.
type family Pred (n :: Nat) :: Nat where
  Pred 0 = 0
  Pred n = (n-1)
