module Data.Monoid.MonadicEndo where

import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

{- |
Like Data.Monoid.Endo but with monadic result.
'mempty' is 'return' and 'mappend' is '<=<'.

Useful e.g. for handling options with GetOpt.
-}
newtype T m a = Cons {run :: a -> m a}


instance Monad m => Semigroup (T m a) where
   Cons x <> Cons y =
      Cons $ (x =<<) . y
--      Cons $ x <=< y

instance Monad m => Monoid (T m a) where
   mempty = Cons return
   mappend = (<>)
