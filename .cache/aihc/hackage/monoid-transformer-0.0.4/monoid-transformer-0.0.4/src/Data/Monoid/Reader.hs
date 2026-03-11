module Data.Monoid.Reader where

import qualified Data.Monoid.Transformer as MonoidTrans
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )
import Data.Functor (Functor, fmap, )
import Data.Function (const, ($), (.), )

import Prelude ()


{- |
Could also be written as @Monoid.Applicative (Monad.Trans.Reader r) a@.
-}
newtype T r a = Cons {run :: r -> a}

pure :: a -> T r a
pure = Cons . const

instance Semigroup a => Semigroup (T r a) where
   Cons x <> Cons y = Cons $ \r -> x r <> y r

instance Monoid a => Monoid (T r a) where
   mempty = MonoidTrans.lift mempty
   mappend (Cons x) (Cons y) =
      Cons $ \r -> mappend (x r) (y r)

instance MonoidTrans.C (T r) where
   lift = Cons . const

instance Functor (T r) where
   fmap f (Cons g) = Cons (f . g)
