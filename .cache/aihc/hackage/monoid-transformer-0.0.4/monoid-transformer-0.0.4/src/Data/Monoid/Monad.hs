module Data.Monoid.Monad where

import qualified Data.Monoid.Transformer as MonoidTrans

import Control.Monad (liftM2, )
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

{- |
Sequence actions and combine their monadic results with 'mappend'.

This type could be omitted, if 'Monad' would be a sub-class of 'Applicative'.
-}
newtype T m a = Cons {run :: m a}


instance (Monad m, Semigroup a) => Semigroup (T m a) where
   Cons x <> Cons y = Cons $ liftM2 (<>) x y

instance (Monad m, Monoid a) => Monoid (T m a) where
   mempty = Cons $ return mempty
   mappend (Cons x) (Cons y) =
      Cons $ liftM2 mappend x y

instance (Monad m) => MonoidTrans.C (T m) where
   lift = Cons . return
