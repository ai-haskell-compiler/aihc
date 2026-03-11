{- |
If you have a 'Trav.Traversable' instance of a record,
you can load and store all elements,
that are accessible by @Traversable@ methods.
We treat the record like an array,
that is we assume, that all elements have the same size and alignment.

Example:

> import Foreign.Storable.Traversable as Store
>
> data Stereo a = Stereo {left, right :: a}
>
> instance Functor Stereo where
>    fmap = Trav.fmapDefault
>
> instance Foldable Stereo where
>    foldMap = Trav.foldMapDefault
>
> instance Traversable Stereo where
>    sequenceA ~(Stereo l r) = liftA2 Stereo l r
>
> instance (Storable a) => Storable (Stereo a) where
>    sizeOf = Store.sizeOf
>    alignment = Store.alignment
>    peek = Store.peek (error "instance Traversable Stereo is lazy, so we do not provide a real value here")
>    poke = Store.poke

You would certainly not define the 'Trav.Traversable' and according instances
just for the implementation of the 'Storable' instance,
but there are usually similar applications
where the @Traversable@ instance is useful.
-}
module Foreign.Storable.Traversable (
   alignment, sizeOf,
   peek, poke,
   peekApplicative,
   ) where

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Control.Applicative as App

-- ToDo: Maybe we should use State.Strict instead?
import Control.Monad.Trans.State
          (StateT, evalStateT, get, put, modify, )
import Control.Monad.IO.Class (liftIO, )

import Foreign.Storable.FixedArray (roundUp, )
import qualified Foreign.Storable as St

import Foreign.Ptr (Ptr, castPtr, )
import Foreign.Storable (Storable, )
import Foreign.Marshal.Array (advancePtr, )


{-# INLINE elementType #-}
elementType :: f a -> a
elementType _ =
   error "Storable.Traversable.alignment and sizeOf may not depend on element values"

{-# INLINE alignment #-}
alignment ::
   (Fold.Foldable f, Storable a) =>
   f a -> Int
alignment = St.alignment . elementType

{-# INLINE sizeOf #-}
{- |
Warning:
It uses Foldable class and will certainly access the data structure
and thus will fail on 'undefined'.

You may call it on the record, after re-constructing it lazily:

> sizeOf . lazy
>
> lazy :: Complex a -> Complex a
> lazy ~(r:+i) = r:+i
-}
sizeOf ::
   (Fold.Foldable f, Storable a) =>
   f a -> Int
sizeOf f =
   Fold.foldl' (\s _ -> s + 1) 0 f *
   roundUp (alignment f) (St.sizeOf (elementType f))


{- |
@peek skeleton ptr@ fills the @skeleton@ with data read from memory beginning at @ptr@.
The skeleton is needed formally for using 'Trav.Traversable'.
For instance when reading a list, it is not clear,
how many elements shall be read.
Using the skeleton you can give this information
and you also provide information that is not contained in the element type @a@.
For example you can call

> peek (replicate 10 ()) ptr

for reading 10 elements from memory starting at @ptr@.
-}
{-# INLINE peek #-}
peek ::
   (Trav.Traversable f, Storable a) =>
   f () -> Ptr (f a) -> IO (f a)
peek skeleton =
   evalStateT (Trav.mapM (const peekState) skeleton) .
   castPtr

{- |
Like 'peek' but uses 'pure' for construction of the result.
'pure' would be in class @Pointed@ if that would exist.
Thus we use the closest approximate 'Applicative'.
-}
{-# INLINE peekApplicative #-}
peekApplicative ::
   (App.Applicative f, Trav.Traversable f, Storable a) =>
   Ptr (f a) -> IO (f a)
peekApplicative =
   evalStateT (Trav.sequence (App.pure peekState)) . castPtr

{-# INLINE peekState #-}
peekState ::
   (Storable a) =>
   StateT (Ptr a) IO a
peekState =
   get >>= \p -> put (advancePtr p 1) >> liftIO (St.peek p)

{-# INLINE poke #-}
poke ::
   (Fold.Foldable f, Storable a) =>
   Ptr (f a) -> f a -> IO ()
poke ptr x =
   evalStateT (Fold.traverse_ pokeState x) $
   castPtr ptr

{-# INLINE pokeState #-}
pokeState ::
   (Storable a) =>
   a -> StateT (Ptr a) IO ()
pokeState x = do
   liftIO . flip St.poke x =<< get
   modify (flip advancePtr 1)
