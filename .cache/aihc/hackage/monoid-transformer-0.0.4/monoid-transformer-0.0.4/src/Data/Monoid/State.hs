module Data.Monoid.State where

import qualified Data.Monoid.Transformer as MonoidTrans
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )
import Data.Functor (Functor, fmap, )
import Data.Function (const, ($), (.), )
import Data.Tuple (fst, snd, )

import Prelude ()

{- |
This resembles the pure State monad.
However, State in transformers is a StateT
and mtl is not Haskell 98.

I hope I have the more natural parameter order for 'evaluate'
in contrast to @mtl@ and @transformers@.
However, it is different from the parameter order of 'run'.

Could also be written as @Monoid.Applicative (Monad.Trans.State s) a@.
-}
newtype T s a = Cons {run :: s -> (a,s)}

pure :: a -> T s a
pure a = Cons $ \s -> (a,s)


evaluate :: s -> T s a -> a
evaluate s m = fst $ run m s

execute :: s -> T s a -> s
execute s m = snd $ run m s


put :: Monoid a => s -> T s a
put s = Cons $ const (mempty, s)

modify :: Monoid a => (s -> s) -> T s a
modify f = Cons $ \s -> (mempty, f s)


instance Semigroup a => Semigroup (T s a) where
   Cons x <> Cons y =
      Cons $ \s0 ->
         let (xr,s1) = x s0
             (yr,s2) = y s1
         in  (xr<>yr, s2)

instance Monoid a => Monoid (T s a) where
   mempty = MonoidTrans.lift mempty
   mappend (Cons x) (Cons y) =
      Cons $ \s0 ->
         let (xr,s1) = x s0
             (yr,s2) = y s1
         in  (mappend xr yr, s2)

instance MonoidTrans.C (T s) where
   lift x = Cons $ (,) x

instance Functor (T s) where
   fmap f (Cons g) = Cons (mapFst f . g)

-- from utility-ht
{-# INLINE mapFst #-}
mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f ~(a,b) = (f a, b)
