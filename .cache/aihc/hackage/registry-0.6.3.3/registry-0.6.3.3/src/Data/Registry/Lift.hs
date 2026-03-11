{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- |
--  This code is taken from https://stackoverflow.com/questions/28003135/is-it-possible-to-encode-a-generic-lift-function-in-haskell
--
--  to allow a generic lift operation over an 'Applicative' context
--  So if you have a function: @Int -> Text -> IO Int@, it can be lifted to have all of its parameters
--    in 'IO':
--
--  >  f :: Int -> Text -> IO Int
--  >
--  >  lifted :: IO Int -> IO Text -> IO Int
--  >  lifted = to @IO f
module Data.Registry.Lift where

import Protolude hiding (Nat)
import Data.Type.Equality

-- | Typeclass for lifting pure functions to effectful arguments and results
class Applicative f => ApplyVariadic f a b where
  applyVariadic :: f a -> b

instance {-# OVERLAPPABLE #-} (Applicative f, b ~ f a) => ApplyVariadic f a b where
  applyVariadic = identity

instance (Monad f, b ~ f a) => ApplyVariadic f (f a) b where
  applyVariadic = join

instance {-# OVERLAPPING #-} (Applicative f, ApplyVariadic f a' b', b ~ (f a -> b')) => ApplyVariadic f (a -> a') b where
  applyVariadic f fa = applyVariadic (f <*> fa)

-- | Lift a pure function to effectful arguments and results
allTo :: forall f a b. ApplyVariadic f a b => a -> b
allTo a = (applyVariadic :: f a -> b) (pure a)

-- | Typeclass for lifting impure functions to effectful arguments and results
class Monad f => ApplyVariadic1 f a b where
  applyVariadic1 :: f a -> b

instance (Monad f, b ~ f a) => ApplyVariadic1 f (f a) b where
  applyVariadic1 = join

instance (Monad f, ApplyVariadic1 f a' b', b ~ (f a -> b')) => ApplyVariadic1 f (a -> a') b where
  applyVariadic1 f fa = applyVariadic1 (f <*> fa)

-- | Lift an effectful function to effectful arguments and results
argsTo :: forall f a b. ApplyVariadic1 f a b => a -> b
argsTo a = (applyVariadic1 :: f a -> b) (pure a)

-- | Typeclass for lifting a function with a result of type m b into a function
--   with a result of type n b
class ApplyVariadic2 f g a b where
  applyVariadic2 :: (forall x. f x -> g x) -> a -> b

instance (b ~ g a) => ApplyVariadic2 f g (f a) b where
  applyVariadic2 natfg = natfg

instance (ApplyVariadic2 f g a' b', b ~ (a -> b')) => ApplyVariadic2 f g (a -> a') b where
  applyVariadic2 natfg f a = applyVariadic2 natfg (f a)

-- | Lift a function returning an effectful result to a function returning another effectful result
outTo :: forall g f a b. ApplyVariadic2 f g a b => (forall x. f x -> g x) -> a -> b
outTo natfg = applyVariadic2 natfg :: a -> b

-- *  Tagging

-- | The output of some constructors can be "tagged" with a string to indicate how a given
--   value was built.
newtype Tag (s :: Symbol) a = Tag {unTag :: a} deriving (Eq, Show)

instance Functor (Tag s) where
  fmap f (Tag a) = Tag @s (f a)

instance Applicative (Tag s) where
  pure = Tag
  Tag f <*> Tag a = Tag @s (f a)

-- | Tag a given constructor f with a string s. The 'applyLast' function only applies the tag to the output
--   type of the constructor. For example
--   data Salary = Fixed Int | Variable Int Double
--   tag @"Variable" Variable :: Int -> Double -> Tag "Variable" Salary
tag :: forall (s :: Symbol) fun. (CNumArgs (CountArgs fun) fun) => fun -> Apply (Tag s) (CountArgs fun) fun
tag = applyLast @(Tag s)

-- | ApplyLast typeclass provided by @neongreen
--   It uses an auxiliary typeclass to count the arguments of a function
data Nat = Z | S Nat

-- | Number of arguments for a given function type
data NumArgs :: Nat -> Type -> Type where
  NAZ :: NumArgs Z a
  NAS :: NumArgs n b -> NumArgs (S n) (a -> b)

-- | Count the number of arguments for a function type
type family CountArgs (f :: Type) :: Nat where
  CountArgs (a -> b) = S (CountArgs b)
  CountArgs result = Z

-- | Typeclass for counting the number of arguments of a function type
class CNumArgs (numArgs :: Nat) (arrows :: Type) where
  getNA :: NumArgs numArgs arrows

-- | Instance for zero arguments
instance CNumArgs Z a where
  getNA = NAZ

-- | Instance for n arguments
instance CNumArgs n b => CNumArgs (S n) (a -> b) where
  getNA = NAS getNA

-- | Type family for applying a function to the last type of a function type
type family Apply (f :: Type -> Type) (n :: Nat) (arrows :: Type) :: Type where
  Apply f (S n) (a -> b) = a -> Apply f n b
  Apply f Z a = f a

-- | Apply a function to the last return value of a function
applyLast :: forall f fun. (Applicative f, CNumArgs (CountArgs fun) fun) => fun -> Apply f (CountArgs fun) fun
applyLast = applyLast' @f (getNA :: NumArgs (CountArgs fun) fun)
  where
    applyLast' :: forall f' n fun'. Applicative f' => NumArgs n fun' -> fun' -> Apply f' n fun'
    applyLast' NAZ x = pure x
    applyLast' (NAS n) f = applyLast' @f' n . f
