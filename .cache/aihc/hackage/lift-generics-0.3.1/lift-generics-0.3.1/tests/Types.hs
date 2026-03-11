{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE StandaloneDeriving         #-}

#if !MIN_VERSION_containers(0,5,8)
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

{-|
Module:      Types
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

Data types for testing `lift-generics`' capabilities.
-}
module Types (
    PureQ, runPureQ
  , Unit(..)
  , Product(..)
  , Sum(..)
  , Const(..)
  , Ap(..)
  , p, s, u, c, z, w
  ) where

import Data.Tree (Tree (..))

import Control.Monad.State

import GHC.Generics (Generic)
import GHC.Exts

import Language.Haskell.TH.Lift.Generics
  ( genericLift
#if MIN_VERSION_template_haskell(2,16,0)
  , genericLiftTypedCompat
#endif
  )
import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat

newtype PureQ a = MkPureQ (State Uniq a)
  deriving (Functor, Applicative, Monad, MonadState Uniq)

runPureQ :: PureQ a -> a
runPureQ m = case m of MkPureQ m' -> evalState m' 0

instance Quote PureQ where
  newName str = state $ \i -> (mkNameU str i, i + 1)

#if !MIN_VERSION_containers(0,5,8)
deriving instance Generic (Tree a)
#endif

data Unit = Unit
  deriving (Eq, Generic, Ord, Show)

data Product a b c d = Product a b c d
  deriving (Eq, Generic, Ord, Show)

data Sum a b = Inl a | Inr b
  deriving (Eq, Generic, Ord, Show)

newtype Const a b = Const a
  deriving (Eq, Generic, Ord, Show)

newtype Ap f a = Ap (f a)
  deriving (Eq, Generic, Ord, Show)

data Unboxed a = Unboxed a Char# Double# Float# Int# Word#
  deriving (Eq, Generic, Ord, Show)

p :: Product Char Int Bool String
p = Product 'a' 1 True "b"

s :: Sum Char Int
s = Inl 'c'

u :: Unboxed Int
u = Unboxed 1 '1'# 1.0## 1.0# 1# 1##

c :: Const Int a
c = Const 1

z :: Ap Maybe Int
z = Ap (Just 3)

w :: Tree Int
w = Node 3 [Node 4 [], Node 5 []]

instance Lift Unit where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

instance (Lift a, Lift b, Lift c, Lift d) => Lift (Product a b c d) where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

instance (Lift a, Lift b) => Lift (Sum a b) where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

instance Lift a => Lift (Unboxed a) where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

-- This instance shows that we can have parametric polymorphism—we don't need
-- Typeable instances for @a@ or @b@.
instance Lift a => Lift (Const a b) where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

-- This instance demonstrates some polykindedness—the arguments to Ap aren't
-- all of kind Type.
instance Lift (f a) => Lift (Ap f a) where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif
