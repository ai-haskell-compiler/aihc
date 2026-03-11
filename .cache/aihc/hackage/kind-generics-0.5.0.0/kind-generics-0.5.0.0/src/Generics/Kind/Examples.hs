{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# language DataKinds             #-}
{-# language DeriveGeneric         #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes            #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UnboxedTuples         #-}
{-# language UndecidableInstances  #-}
module Generics.Kind.Examples where

import           Data.Kind
import           GHC.Generics    (Generic)
import           GHC.TypeLits
import           Type.Reflection (Typeable)

import qualified Fcf.Core as Fcf
import           Generics.Kind

-- Obtained from Generic

instance GenericK Maybe where
  type RepK Maybe = U1 :+: Field Var0
instance GenericK (Maybe a) where
  type RepK (Maybe a) = SubstRep (RepK Maybe) a
  fromK = fromRepK
  toK   = toRepK

instance GenericK [] where
  type RepK [] = U1 :+: Field Var0 :*: Field ([] :$: Var0)
instance GenericK [a] where
  type RepK [a] = SubstRep (RepK []) a
  fromK = fromRepK
  toK   = toRepK

instance GenericK Either where
  type RepK Either = Field Var0 :+: Field Var1
instance GenericK (Either a) where
  type RepK (Either a) = SubstRep (RepK Either) a
  fromK = fromRepK
  toK   = toRepK
instance GenericK (Either a b) where
  type RepK (Either a b) = SubstRep (RepK (Either a)) b
  fromK = fromRepK
  toK   = toRepK

-- From the docs

data Tree a = Branch (Tree a) (Tree a) | Leaf a
            deriving Generic

instance GenericK Tree where
  type RepK Tree = Field (Tree :$: Var0) :*: Field (Tree :$: Var0) :+: Field Var0
instance GenericK (Tree a) where
  type RepK (Tree a) = SubstRep (RepK Tree) a
  fromK = fromRepK
  toK   = toRepK

-- Data family

data family HappyFamily t
data instance HappyFamily (Maybe a) = HFM Bool
data instance HappyFamily [a]       = HFL a

instance GenericK HappyFamily where
  type RepK HappyFamily = TypeError ('Text "Cannot describe this family uniformly")
  fromK = undefined
  toK   = undefined

instance GenericK (HappyFamily (Maybe a)) where
  type RepK (HappyFamily (Maybe a)) = Field ('Kon Bool)
  fromK (HFM   x) = Field x
  toK   (Field x) = HFM   x

instance GenericK (HappyFamily [a]) where
  type RepK (HappyFamily [a]) = Field ('Kon a)
  fromK (HFL   x) = Field x
  toK   (Field x) = HFL   x

-- Hand-written instance

data SimpleIndex :: Type -> Type -> Type where
  MkSimpleIndex :: [a] -> SimpleIndex [a] b

instance GenericK SimpleIndex where
  type RepK SimpleIndex
    = Exists Type ((Var1 :~: ([] :$: Var0)) :=>: Field ([] :$: Var0))
  fromK (MkSimpleIndex x) = Exists (SuchThat (Field x))
  toK (Exists (SuchThat (Field x))) = MkSimpleIndex x

instance GenericK (SimpleIndex a) where
  type RepK (SimpleIndex a)
    = Exists Type (('Kon a :~: ([] :$: Var0)) :=>: Field ([] :$: Var0))
  fromK (MkSimpleIndex x) = Exists (SuchThat (Field x))
  toK (Exists (SuchThat (Field x))) = MkSimpleIndex x

instance GenericK (SimpleIndex a b) where
  type RepK (SimpleIndex a b)
    = Exists Type (('Kon a :~: ([] :$: Var0)) :=>: Field ([] :$: Var0))
  fromK (MkSimpleIndex x) = Exists (SuchThat (Field x))
  toK (Exists (SuchThat (Field x))) = MkSimpleIndex x

data WeirdTree a where
  WeirdBranch :: WeirdTree a -> WeirdTree a -> WeirdTree a
  WeirdLeaf   :: Show a => t -> a -> WeirdTree a

instance GenericK WeirdTree where
  type RepK WeirdTree
    = Field (WeirdTree :$: Var0) :*: Field (WeirdTree :$: Var0)
      :+: Exists Type ((Show :$: Var1) :=>: (Field Var0 :*: Field Var1))

  fromK (WeirdBranch l r) = L1 $                     Field l :*: Field r
  fromK (WeirdLeaf   a x) = R1 $ Exists $ SuchThat $ Field a :*: Field x

  toK (L1 (Field l :*: Field r))                     = WeirdBranch l r
  toK (R1 (Exists (SuchThat (Field a :*: Field x)))) = WeirdLeaf a x

-- Hand-written instance with reflection

data WeirdTreeR a where
  WeirdBranchR :: WeirdTreeR a -> WeirdTreeR a -> WeirdTreeR a
  WeirdLeafR   :: (Show a, Eq t, Typeable t) => t -> a -> WeirdTreeR a

instance GenericK WeirdTreeR where
  type RepK WeirdTreeR
    = Field (WeirdTreeR :$: Var0) :*: Field (WeirdTreeR :$: Var0)
      :+: Exists Type (((Show :$: Var1) ':&: (Eq :$: Var0) ':&: (Typeable :$: Var0))
                       :=>: (Field Var0 :*: Field Var1))

  fromK (WeirdBranchR l r) = L1 $                     Field l :*: Field r
  fromK (WeirdLeafR   a x) = R1 $ Exists $ SuchThat $ Field a :*: Field x

  toK (L1 (Field l :*: Field r))                     = WeirdBranchR l r
  toK (R1 (Exists (SuchThat (Field a :*: Field x)))) = WeirdLeafR a x

instance GenericK (WeirdTreeR a) where
  type RepK (WeirdTreeR a)
    = Field ('Kon (WeirdTreeR a)) :*: Field ('Kon (WeirdTreeR a))
    :+: Exists Type (('Kon (Show a) ':&: (Eq :$: Var0) ':&: (Typeable :$: Var0))
                     :=>: (Field Var0 :*: Field ('Kon a)))

  fromK (WeirdBranchR l r) = L1 $                     Field l :*: Field r
  fromK (WeirdLeafR   a x) = R1 $ Exists $ SuchThat $ Field a :*: Field x

  toK (L1 (Field l :*: Field r))                     = WeirdBranchR l r
  toK (R1 (Exists (SuchThat (Field a :*: Field x)))) = WeirdLeafR a x

-- From https://gitlab.com/trupill/kind-generics/issues/3

data TTY m a where
  WriteTTY :: String -> TTY m ()
  ReadTTY  :: TTY m String

instance GenericK (TTY m a) where
  type RepK (TTY m a)
    =     (('Kon a :~: 'Kon ()) :=>: Field ('Kon String))
      :+: (('Kon a :~: 'Kon String) :=>: U1)

  fromK (WriteTTY s) = L1 (SuchThat (Field s))
  fromK ReadTTY      = R1 (SuchThat U1)

  toK (L1 (SuchThat (Field s))) = WriteTTY s
  toK (R1 (SuchThat U1))        = ReadTTY

-- Weird-kinded types

data T (a :: k) where
  MkT :: forall (a :: Type). Maybe a -> T a

{- GHC rewrites this to the following Core
data T (a :: k) =
  forall (a' :: Type). (k ~ Type, a ~~ a') => MkT (Maybe a')
-}

instance GenericK (T :: k -> Type) where
  type RepK (T :: k -> Type) =
    Exists Type (('Kon (k ~ Type) ':&: (Var0 :~~: Var1)) :=>: Field (Maybe :$: Var0))
  fromK (MkT x) = Exists (SuchThat (Field x))
  toK (Exists (SuchThat (Field x))) = MkT x

data P k (a :: k) where
  P :: forall k (a :: k). P k a

instance GenericK (P k) where
  type RepK (P k) = U1
  fromK P  = U1
  toK   U1 = P

{- This does not work
instance GenericK P (k :&&: a :&&: LoT0) where
  type RepK P = KindOf Var1 :~: Var0 :=>: U1
-}

data P' j (a :: k) where
  P' :: forall k (a :: k). P' k a

instance GenericK (P' j :: k -> Type) where
  type RepK (P' j :: k -> Type) = ('Kon k :~: 'Kon j) :=>: U1
  fromK P' = SuchThat U1
  toK (SuchThat U1) = P'

instance GenericK (P' :: Type -> k -> Type) where
  type RepK (P' :: Type -> k -> Type) = ('Kon k :~: Var0) :=>: U1
  fromK P' = SuchThat U1
  toK (SuchThat U1) = P'

-- Rank-N types

newtype Ranky = MkRanky (forall a. a -> a)

instance GenericK Ranky where
  type RepK Ranky = Field ('ForAll ((->) :$: Var0 ':@: Var0))
  fromK (MkRanky x) = Field (ForAllI x)
  toK (Field (ForAllI x)) = MkRanky x

newtype Ranky2 b = MkRanky2 ((forall a. a -> a) -> b)

instance GenericK Ranky2 where
  type RepK Ranky2 = Field ((->) :$: 'ForAll ((->) :$: Var0 ':@: Var0) ':@: Var0)
  fromK (MkRanky2 f) = Field (\(ForAllI x) -> f x)
  toK (Field f) = MkRanky2 (\x -> f (ForAllI x))

data Shower a where
  MkShower :: (Show a => a -> String) -> Shower a

instance GenericK Shower where
  type RepK Shower = Field ((Show :$: Var0) ':=>>: ((->) :$: Var0 ':@: 'Kon String))
  fromK (MkShower f) = Field (SuchThatI f)
  toK (Field (SuchThatI f)) = MkShower f

-- Other representation types

data Unboxed1 = MkUnboxed1 (# Int, Int #)
{- -- We cannot write this
instance GenericK Unboxed1 'LoT0 where
  type RepK Unboxed1 = Field (Kon (# Int, Int #))
  -- fromK (MkUnboxed1 x) = Field x
  -- toK (Field x) = MkUnboxed1 x
  fromK = undefined
  toK   = undefined
-}

-- Type families

-- HKD pattern, defunctionalized variant
newtype Hkd f a = Hkd (Fcf.Eval (f a))

-- Simplified encoding compared to kind-generics-th.
-- Fcf.Eval in Hkd could be any other type family, so a general
-- implementation would have to defunctionalize it.
instance GenericK Hkd where
  type RepK Hkd = Field (Eval (Var0 :@: Var1))
  fromK (Hkd x) = Field x
  toK (Field x) = Hkd x

instance GenericK (Hkd f) where
  type RepK (Hkd f) = Field (Eval (Kon f :@: Var0))
  fromK (Hkd x) = Field x
  toK (Field x) = Hkd x

instance GenericK (Hkd f a) where
  type RepK (Hkd f a) = Field (Kon (Fcf.Eval (f a)))
  fromK (Hkd x) = Field x
  toK (Field x) = Hkd x
