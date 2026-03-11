module NumericPrelude.Elementwise where

import Control.Applicative (Applicative(pure, (<*>)), )

{- |
A reader monad for the special purpose
of defining instances of certain operations on tuples and records.
It does not add any new functionality to the common Reader monad,
but it restricts the functions to the required ones
and exports them from one module.
That is you do not have to import
both Control.Monad.Trans.Reader and Control.Applicative.
The type also tells the user, for what the Reader monad is used.
We can more easily replace or extend the implementation when needed.
-}
newtype T v a = Cons {run :: v -> a}

{-# INLINE with #-}
with :: a -> T v a
with e = Cons $ \ _v -> e

{-# INLINE element #-}
element :: (v -> a) -> T v a
element = Cons


{-# INLINE run2 #-}
run2 :: T (x,y) a -> x -> y -> a
run2 = curry . run

{-# INLINE run3 #-}
run3 :: T (x,y,z) a -> x -> y -> z -> a
run3 e x y z = run e (x,y,z)

{-# INLINE run4 #-}
run4 :: T (x,y,z,w) a -> x -> y -> z -> w -> a
run4 e x y z w = run e (x,y,z,w)

{-# INLINE run5 #-}
run5 :: T (x,y,z,u,w) a -> x -> y -> z -> u -> w -> a
run5 e x y z u w = run e (x,y,z,u,w)


instance Functor (T v) where
   {-# INLINE fmap #-}
   fmap f (Cons e) =
      Cons $ \v -> f $ e v

instance Applicative (T v) where
   {-# INLINE pure #-}
   {-# INLINE (<*>) #-}
   pure = with
   Cons f <*> Cons e =
      Cons $ \v -> f v $ e v
