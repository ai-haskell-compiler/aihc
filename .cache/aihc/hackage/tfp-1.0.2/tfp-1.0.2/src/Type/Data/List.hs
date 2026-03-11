{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Type.Data.List
    ( Cons
    , Null
    , IsNull
    , Head
    , Tail
    , Reverse
    , Append
    ) where

import qualified Prelude

import Data.Typeable

import Type.Data.Bool

data Cons car cdr deriving (Typeable)
instance (Prelude.Show car, Prelude.Show cdr) => Prelude.Show (Cons car cdr) where
    show = showCons

showCons :: forall car cdr . (Prelude.Show car, Prelude.Show cdr) => Cons car cdr -> Prelude.String
showCons _ = "Cons (" Prelude.++ Prelude.show (Prelude.undefined :: car) Prelude.++ ") (" Prelude.++ Prelude.show (Prelude.undefined :: cdr) Prelude.++ ")"

data Null deriving (Typeable)
instance Prelude.Show Null where
    show _ = ""

type family IsNull l
type instance IsNull (Cons _car _cdr) = False
type instance IsNull Null = True

type family Head l
type instance Head (Cons car _cdr) = car

type family Tail l
type instance Tail (Cons _car cdr) = cdr

type family Reverse l
type instance Reverse l = Reverse' l Null

type family Reverse' l a
type instance Reverse' Null a = a
type instance Reverse' (Cons car cdr) a = Reverse' cdr (Cons car a)

type family Append l1 l2
type instance Append Null l2 = l2
type instance Append (Cons car1 cdr2) l2 = Cons car1 (Append cdr2 l2)
