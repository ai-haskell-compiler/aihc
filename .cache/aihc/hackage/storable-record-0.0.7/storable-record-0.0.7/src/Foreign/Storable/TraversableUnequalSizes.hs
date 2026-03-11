{- |
If you have a Traversable instance of a record,
you can load and store all elements,
that are accessible by Traversable methods.
In this attempt we support elements of unequal size.
However this can be awfully slow,
since the program might perform size computations at run-time.
-}
module Foreign.Storable.TraversableUnequalSizes (
   alignment, sizeOf,
   peek, poke,
   ) where

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import Control.Monad.Trans.State
          (StateT, evalStateT, gets, modify, )
import Control.Monad.IO.Class (liftIO, )

import Foreign.Storable.FixedArray (roundUp, )
import qualified Foreign.Storable as St

import Foreign.Ptr (Ptr, )
import Foreign.Storable (Storable, )


{-# INLINE alignment #-}
alignment ::
   (Fold.Foldable f, Storable a) =>
   f a -> Int
alignment =
   Fold.foldl' (\n x -> lcm n (St.alignment x)) 1

{-# INLINE sizeOf #-}
sizeOf ::
   (Fold.Foldable f, Storable a) =>
   f a -> Int
sizeOf f =
   roundUp (alignment f) $
   Fold.foldl' (\s x -> roundUp (St.alignment x) s + St.sizeOf x) 0 f

{-
This function requires that alignment does not depend on an element value,
because we cannot not know the value before loading it.
Thus @alignment (undefined::a)@ must be defined.
-}
{-# INLINE peek #-}
peek ::
   (Trav.Traversable f, Storable a) =>
   f () -> Ptr (f a) -> IO (f a)
peek skeleton ptr =
   evalStateT (Trav.mapM (const (peekState ptr)) skeleton) 0

{-# INLINE peekState #-}
peekState ::
   (Storable a) =>
   Ptr (f a) -> StateT Int IO a
peekState p = do
   let pseudoPeek :: Ptr (f a) -> a
       pseudoPeek _ = error "Traversable.peek: alignment must not depend on the element value"
   k <- getOffset (pseudoPeek p)
   a <- liftIO (St.peekByteOff p k)
   advanceOffset a
   return a

{-# INLINE poke #-}
poke ::
   (Fold.Foldable f, Storable a) =>
   Ptr (f a) -> f a -> IO ()
poke ptr x =
   evalStateT (Fold.traverse_ (pokeState ptr) x) 0

{-# INLINE pokeState #-}
pokeState ::
   (Storable a) =>
   Ptr (f a) -> a -> StateT Int IO ()
pokeState p a = do
   k <- getOffset a
   liftIO (St.pokeByteOff p k a)
   advanceOffset a

{-# INLINE getOffset #-}
getOffset ::
   (Storable a) =>
   a -> StateT Int IO Int
getOffset a =
   gets (roundUp (St.alignment a))

{-# INLINE advanceOffset #-}
advanceOffset ::
   (Storable a) =>
   a -> StateT Int IO ()
advanceOffset a =
   modify ( + St.sizeOf a)
