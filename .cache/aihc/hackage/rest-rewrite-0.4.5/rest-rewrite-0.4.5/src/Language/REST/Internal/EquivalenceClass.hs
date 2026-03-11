{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.REST.Internal.EquivalenceClass
    ( isMember
    , isSingleton
    , insert
    , union
    , singleton
    , fromList
    , toList
    , head
    , EquivalenceClass
    , elems
    , isSubsetOf
    ) where

import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.Set as S
import qualified Data.List as L
import Prelude hiding (head)

import Language.REST.Types () -- Hashable (S.Set a)

-- | Equivalent classes of the @(==)@ relation of a type @a@.
newtype EquivalenceClass a =
  -- | The set contains all of the elements of the class
  EquivalenceClass (S.Set a)
#if MIN_VERSION_hashable(1,3,5)
  deriving (Ord, Eq, Generic, Hashable)
#else
  deriving (Ord, Eq, Generic)
#endif

#if !MIN_VERSION_hashable(1,3,5)
deriving instance Hashable (S.Set a) => Hashable (EquivalenceClass a)
#endif

instance Show a => Show (EquivalenceClass a) where
    show (EquivalenceClass xs) = L.intercalate " = " (map show (S.toList xs)) 



{-# INLINE isSubsetOf #-}
isSubsetOf :: Ord a => EquivalenceClass a -> EquivalenceClass a -> Bool
isSubsetOf (EquivalenceClass xs) (EquivalenceClass ys) = xs `S.isSubsetOf` ys

head :: EquivalenceClass a -> a
head (EquivalenceClass xs) = L.head $ S.toList xs

isSingleton :: EquivalenceClass a -> Bool
isSingleton (EquivalenceClass xs) = S.size xs == 1

{-# INLINE isMember #-}
isMember :: (Ord a, Eq a, Hashable a) => a -> EquivalenceClass a -> Bool
isMember x (EquivalenceClass xs) = S.member x xs

insert :: (Ord a, Eq a, Hashable a) => a -> EquivalenceClass a -> EquivalenceClass a
insert x (EquivalenceClass xs) = EquivalenceClass (S.insert x xs)

union :: (Ord a, Eq a, Hashable a) => EquivalenceClass a -> EquivalenceClass a -> EquivalenceClass a
union (EquivalenceClass xs) (EquivalenceClass ys) = 
    EquivalenceClass (S.union xs ys)

singleton :: (Ord a, Eq a, Hashable a) => a -> EquivalenceClass a
singleton = EquivalenceClass . S.singleton

fromList :: (Ord a, Eq a, Hashable a) => [a] -> EquivalenceClass a
fromList = EquivalenceClass . S.fromList

toList :: EquivalenceClass a -> [a]
toList (EquivalenceClass s) = S.toList s

{-# INLINE elems #-}
elems :: EquivalenceClass a -> S.Set a
elems (EquivalenceClass ec) = ec
