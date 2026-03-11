{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      :  Data.Transaction

Copyright   :  Kadzuya Okamoto 2018
License     :  MIT

Stability   :  experimental
Portability :  unknown

Monadic representation of transactions.
-}
module Data.Transaction
  (
  -- * Constructors
    action
  -- * Converters
  , reduce
  , toList
  , tMap
  , tFilter
  , tFilterMap
  -- * Types
  , Transaction
  , TransactionM
  ) where

import Prelude hiding
  ( Foldable(..)
  , all
  , any
  , drop
  , dropWhile
  , filter
  , head
  , init
  , last
  , repeat
  , replicate
  , span
  , tail
  , take
  , takeWhile
  )

import Data.Bifunctor (Bifunctor(..))
import qualified Data.Monoid as Monoid
import Data.MonoTraversable
  ( Element
  , GrowingAppend
  , MonoFoldable(..)
  , MonoFunctor(..)
  , MonoPointed(..)
  , MonoTraversable(..)
  )
import Data.Semigroup as Sem
import Data.Sequences
  ( Index
  , IsSequence(..)
  , SemiSequence(..)
  , defaultSnoc
  , defaultSortBy
  )

{- ==============
 -     Types
 - ============== -}
data TransactionM a x
  = TVal a
         (TransactionM a x)
  | TNull x
  deriving (Functor)

type Transaction a = TransactionM a ()

instance Applicative (TransactionM a) where
  pure = TNull
  TVal a next <*> f = TVal a (next <*> f)
  TNull g <*> f = f >>= (pure . g)

instance Monad (TransactionM a) where
  return = pure
  TVal a next >>= f = TVal a (next >>= f)
  TNull a >>= f = f a

instance Sem.Semigroup (Transaction a) where
  TVal a next <> t = TVal a (next <> t)
  TNull _ <> t = t

instance Monoid (Transaction a) where
  mempty = TNull ()
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance Bifunctor TransactionM where
  first :: forall a b x. (a -> b) -> TransactionM a x -> TransactionM b x
  first _ (TNull a) = pure a
  first f (TVal a next) = TVal (f a) $ first f next
  second :: forall a x y. (x -> y) -> TransactionM a x -> TransactionM a y
  second = fmap

type instance Element (Transaction a) = a

instance MonoFunctor (Transaction a) where
  omap :: (a -> a) -> Transaction a -> Transaction a
  omap = first

instance MonoFoldable (Transaction a) where
  otoList = toList
  ocompareLength :: Integral i => Transaction a -> i -> Ordering
  ocompareLength (TNull ()) i = 0 `compare` i
  ocompareLength (TVal _ next) i
    | i <= 0 = GT
    | otherwise = ocompareLength next (i - 1)
  ofoldMap = foldMap
  ofoldr = foldr
  ofoldl' = foldl'
  ofoldr1Ex = foldr1
  ofoldl1Ex' = foldl1'

foldMap :: (Monoid m) => (a -> m) -> Transaction a -> m
foldMap f = foldr (\x m -> f x Monoid.<> m) mempty

foldr :: (a -> b -> b) -> b -> Transaction a -> b
foldr _ b (TNull ()) = b
foldr f b (TVal a next) = f a (foldr f b next)

foldl :: (b -> a -> b) -> b -> Transaction a -> b
foldl f b (TVal a next) = foldl f (f b a) next
foldl _ b (TNull ()) = b

foldl' :: (b -> a -> b) -> b -> Transaction a -> b
foldl' (??) z xs = (foldr (?!) id xs) z
  where
    x ?! g = g . (?? x)

foldr1 :: (a -> a -> a) -> Transaction a -> a
foldr1 _ (TNull ()) = error "Transaction.foldr1: empty transaction"
foldr1 _ (TVal a (TNull ())) = a
foldr1 f (TVal a next) = f a (foldr1 f next)

foldl1' :: (a -> a -> a) -> Transaction a -> a
foldl1' _ (TNull ()) = error "Transaction.foldl1': empty transaction"
foldl1' f (TVal a next) = foldl' f a next

#if MIN_VERSION_base(4,11,0)
{-# NOINLINE [1] length #-}
length :: Transaction a -> Int
length t = lenAcc t 0

lenAcc :: Transaction a -> Int -> Int
lenAcc (TNull ()) n = n
lenAcc (TVal _ next) n = lenAcc next (n + 1)
#endif

instance MonoPointed (Transaction a) where
  opoint :: a -> Transaction a
  opoint = action

instance SemiSequence (Transaction a) where
  type Index (Transaction a) = Int
  intersperse :: a -> Transaction a -> Transaction a
  intersperse _ (TNull ()) = pure ()
  intersperse sep (TVal a next) = TVal a $ prependToAll sep next
  reverse :: Transaction a -> Transaction a
  reverse = reduce (\t a -> TVal a t) (TNull ())
  find :: (a -> Bool) -> Transaction a -> Maybe a
  find p t =
    case tFilter p t of
      TNull () -> Nothing
      TVal a _ -> Just a
  sortBy :: (a -> a -> Ordering) -> Transaction a -> Transaction a
  sortBy = defaultSortBy
  cons :: a -> Transaction a -> Transaction a
  cons a t = TVal a t
  snoc :: Transaction a -> a -> Transaction a
  snoc = defaultSnoc

prependToAll :: a -> Transaction a -> Transaction a
prependToAll _ (TNull ()) = pure ()
prependToAll sep (TVal a next) = TVal sep $ TVal a $ prependToAll sep next

instance GrowingAppend (Transaction a)

instance MonoTraversable (Transaction a) where
  otraverse :: Applicative f => (a -> f a) -> Transaction a -> f (Transaction a)
  otraverse _ (TNull ()) = pure $ pure ()
  otraverse f (TVal a next) = TVal <$> f a <*> otraverse f next

instance IsSequence (Transaction a) where
  fromList :: [a] -> Transaction a
  fromList [] = pure ()
  fromList (x:xs) = TVal x $ fromList xs

#if MIN_VERSION_mono_traversable(1,0,2)
  lengthIndex :: Transaction a -> Int
  lengthIndex = length
#endif

  {-# NOINLINE [1] filter #-}
  filter :: (a -> Bool) -> Transaction a -> Transaction a
  filter _ (TNull ()) = pure ()
  filter p (TVal a next)
    | p a = TVal a $ filter p next
    | otherwise = filter p next
  filterM :: Monad m => (a -> m Bool) -> Transaction a -> m (Transaction a)
  filterM _ (TNull ()) = pure $ pure ()
  filterM mp (TVal a next) = do
    b <- mp a
    next' <- filterM mp next
    pure $
      if b
        then TVal a next'
        else next'
  break :: (a -> Bool) -> Transaction a -> (Transaction a, Transaction a)
  break p = span (not . p)
  span :: (a -> Bool) -> Transaction a -> (Transaction a, Transaction a)
  span _ t@(TNull ()) = (t, t)
  span p t@(TVal a next)
    | p a =
      let (y, z) = span p next
       in (TVal a y, z)
    | otherwise = (pure (), t)
  dropWhile :: (a -> Bool) -> Transaction a -> Transaction a
  dropWhile _ (TNull ()) = pure ()
  dropWhile p t@(TVal a next)
    | p a = dropWhile p next
    | otherwise = t
  takeWhile :: (a -> Bool) -> Transaction a -> Transaction a
  takeWhile _ (TNull ()) = pure ()
  takeWhile p (TVal a next)
    | p a = TVal a $ takeWhile p next
    | otherwise = pure ()
  splitAt :: Int -> Transaction a -> (Transaction a, Transaction a)
  splitAt n t = (take n t, drop n t)
  take :: Int -> Transaction a -> Transaction a
  take n _
    | n <= 0 = pure ()
  take _ (TNull ()) = pure ()
  take n (TVal a next) = TVal a $ take (n - 1) next
  drop :: Int -> Transaction a -> Transaction a
  drop n t
    | n <= 0 = t
  drop _ (TNull ()) = pure ()
  drop n (TVal _ next) = drop (n - 1) next
  uncons :: Transaction a -> Maybe (a, Transaction a)
  uncons (TNull ()) = Nothing
  uncons (TVal a next) = Just (a, next)
  unsnoc (TNull ()) = Nothing
  unsnoc (TVal a0 next0) = Just (loop id a0 next0)
    where
      loop front a (TNull ()) = (front $ pure (), a)
      loop front a (TVal y z) = loop (front . (TVal a)) y z
  {-# INLINE partition #-}
  partition :: (a -> Bool) -> Transaction a -> (Transaction a, Transaction a)
  partition p t = foldr (select p) (pure (), pure ()) t
  {-# INLINE replicate #-}
  replicate :: Int -> a -> Transaction a
  replicate n a = take n (repeat a)
  replicateM :: Monad m => Int -> m a -> m (Transaction a)
  replicateM cnt0 f = loop cnt0
    where
      loop cnt
        | cnt <= 0 = pure $ pure ()
        | otherwise = TVal <$> f <*> loop (cnt - 1)

{-# INLINE [0] repeat #-}
repeat :: a -> Transaction a
repeat a = t
  where
    t = TVal a t

select ::
     (a -> Bool)
  -> a
  -> (Transaction a, Transaction a)
  -> (Transaction a, Transaction a)
select p x ~(ts, fs)
  | p x = (TVal x ts, fs)
  | otherwise = (ts, TVal x fs)

{- ==============
 -   Operators
 - ============== -}
{- |
>>> :{
toList $ do
  action 4
  action 5
  action 6
:}
[4,5,6]

>>> :{
toList $ filter even $ do
  action 4
  action 5
  action 6
:}
[4,6]
-}
action :: a -> Transaction a
action a = TVal a $ pure ()

{- ==============
 -   Converters
 - ============== -}
{- | An alias of 'first' for convenience.

>>> :{
toList $ do
  action 4
  tMap (+1) $ do
    action 5
    action 6
  action 7
:}
[4,6,7,7]
-}
tMap :: (a -> b) -> Transaction a -> Transaction b
tMap f = tFilterMap (pure . f)

{-# DEPRECATED
tFilter "Use `IsSequence.filter` instead."
 #-}

{- | An alias of 'filter'.

>>> :{
toList $ do
  action 4
  tFilter even $ do
    action 5
    action 6
  action 7
:}
[4,6,7]

>>> :{
toList $ do
  action 4
  filter even $ do
    action 5
    action 6
  action 7
:}
[4,6,7]

-}
tFilter :: (a -> Bool) -> Transaction a -> Transaction a
tFilter = filter

{-# DEPRECATED
tFilterMap "This will be removed in a future release."
 #-}

{- |
>>> :{
toList $ do
  action 4
  tFilterMap (\x -> if even x then Just (x + 1) else Nothing) $ do
    action 5
    action 6
  action 7
:}
[4,7,7]
-}
tFilterMap :: (a -> Maybe b) -> Transaction a -> Transaction b
tFilterMap f (TVal a next) =
  case f a of
    Just b -> TVal b $ tFilterMap f next
    Nothing -> tFilterMap f next
tFilterMap _ (TNull ()) = TNull ()

{- | An alias of 'foldl' for convenience.
-}
reduce :: (b -> a -> b) -> b -> Transaction a -> b
reduce = foldl

toList :: Transaction a -> [a]
toList trans = reduce (\f a -> f . (a :)) id trans []
