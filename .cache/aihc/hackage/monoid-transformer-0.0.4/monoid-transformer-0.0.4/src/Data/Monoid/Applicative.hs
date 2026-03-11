module Data.Monoid.Applicative where

import qualified Data.Monoid.Transformer as MonoidTrans

import Control.Applicative (Applicative, pure, liftA2, )
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

{- |
Sequence applicative functors and combine their functorial results with 'mappend'.
-}
newtype T f a = Cons {run :: f a}


instance (Applicative f, Semigroup a) => Semigroup (T f a) where
   Cons x <> Cons y = Cons $ liftA2 (<>) x y

instance (Applicative f, Monoid a) => Monoid (T f a) where
   mempty = Cons $ pure mempty
   mappend (Cons x) (Cons y) =
      Cons $ liftA2 mappend x y

instance (Applicative f) => MonoidTrans.C (T f) where
   lift = Cons . pure
