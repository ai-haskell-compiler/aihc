{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module HaskellWorks.Data.MQuery where

import Control.Lens
import Control.Monad
import Data.List
import GHC.Base
import HaskellWorks.Data.MQuery.Entry
import HaskellWorks.Data.MQuery.Row
import HaskellWorks.Data.MQuery.ToBool
import Prettyprinter

import qualified Data.DList as DL

newtype MQuery a = MQuery (DL.DList a)

deriving instance Functor     MQuery
deriving instance Applicative MQuery
deriving instance Monad       MQuery
deriving instance Alternative MQuery
deriving instance MonadPlus   MQuery
deriving instance Foldable    MQuery

class IsPredicate a where
  type ArgOf a
  toPredicate :: ArgOf a -> (a -> Bool)

mQuery :: MQuery a -> DL.DList a
mQuery (MQuery a) = a

instance ToBool (MQuery a) where
  toBool = toBool . mQuery

instance Pretty (MQuery String) where
  pretty x = prettyRowOfString (Row 120 (mQuery x))

instance Pretty (MQuery Integer) where
  pretty x = prettyRowOfString (Row 120 (mQuery x))

instance Pretty (MQuery Int) where
  pretty x = prettyRowOfString (Row 120 (mQuery x))

satisfying :: (a -> Bool) -> a -> MQuery a
satisfying p a | p a  = MQuery $ DL.singleton a
satisfying _ _ = MQuery   DL.empty

key :: Entry k v -> MQuery k
key (Entry k _) = MQuery $ DL.singleton k

value :: Entry k v -> MQuery v
value (Entry _ v) = MQuery $ DL.singleton v

dlTake :: Int -> DL.DList a -> DL.DList a
dlTake n = DL.fromList . take n . DL.toList

select :: ToBool b => a -> (a -> b) -> MQuery a
select a f = if toBool (f a) then MQuery (DL.singleton a) else MQuery DL.empty

having :: (a -> MQuery b) -> a -> MQuery a
having p a = case p a of
  MQuery das -> case DL.toList das of
    _:_ -> MQuery (DL.singleton a)
    _   -> MQuery  DL.empty

valueOf :: Eq a => a -> a -> MQuery a
valueOf a b = if a == b then MQuery (DL.singleton b) else MQuery DL.empty

limit :: Int -> MQuery a -> MQuery a
limit n (MQuery xs) = MQuery ((DL.fromList . take n . DL.toList) xs)

skip :: Int -> MQuery a -> MQuery a
skip n (MQuery xs) = MQuery ((DL.fromList . drop n . DL.toList) xs)

page :: Int -> Int -> MQuery a -> MQuery a
page size n (MQuery xs) = MQuery ((DL.fromList . take size . drop (size * n) . DL.toList) xs)

sorted :: Ord a => MQuery a -> MQuery a
sorted (MQuery xs) = MQuery ((DL.fromList . sort . DL.toList) xs)

onList :: ([a] -> [a]) -> MQuery a -> MQuery a
onList f (MQuery xs) = MQuery ((DL.fromList . f . DL.toList) xs)

count :: MQuery a -> MQuery Int
count (MQuery xs) = MQuery (DL.singleton (length (DL.toList xs)))

aggregate :: ([a] -> b) -> MQuery a -> MQuery b
aggregate f (MQuery xs) = MQuery (DL.fromList [f (DL.toList xs)])

uniq :: Eq a => [a] -> [a]
uniq (a:b:cs) | a == b  =   uniq (b:cs)
uniq (a:b:cs) = a:uniq (b:cs)
uniq cs       = cs

infixl 1 >>^.
infixl 1 >>^..

instance Semigroup (MQuery a) where
  MQuery a <> MQuery b = MQuery (a `DL.append` b)

instance Monoid (MQuery a) where
  mempty = MQuery DL.empty

(/^.) :: Monad m => s -> Getting a s a -> m a
(/^.) a g = return (a ^. g)

(/^..) :: (Monad m, Foldable t, Monoid (m a)) => s -> Getting (t a) s (t a) -> m a
(/^..) a g = foldMap return (a ^. g)

(>>^.) :: Monad m => m a -> Getting b a b -> m b
(>>^.) q g = q >>= (/^. g)

(>>^..) :: (Monad m, Foldable t, Monoid (m a), Monoid (m b)) => m a -> Getting (t b) a (t b) -> m b
(>>^..) q g = q >>= (/^.. g)
