{-# LANGUAGE Rank2Types #-}
{- |
Build a lazy storable vector by incrementally adding an element.
This is analogous to Data.Binary.Builder for Data.ByteString.Lazy.

Attention:
This implementation is still almost 3 times slower
than constructing a lazy storable vector using unfoldr
in our Chorus speed test.
-}
module Data.StorableVector.Lazy.Builder (
   Builder,
   toLazyStorableVector,
   put,
   flush,
   ) where

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector.ST.Strict as STV
-- import qualified Data.StorableVector.ST.Lazy as STVL

import Data.StorableVector.Lazy (ChunkSize, )
import Control.Monad (liftM2, )
import Control.Monad.ST.Strict (ST, runST, )
import Data.Monoid (Monoid(mempty, mappend), )
import Data.Semigroup (Semigroup((<>)), )

import Foreign.Storable (Storable, )

import qualified System.Unsafe as Unsafe


{-
Given an initial buffer and a function that generates the rest of the vector,
a 'Builder' generates the whole vector.
The idea is inspired by Data.Binary.Builder.

We use the strict ST monad by default
and only rare 'Unsafe.interleaveST',
since this is more efficient than using lazy ST everywhere.

Before that approach I tried to achieve this with a lazy State monad.
I found this more comprehensible but it was very slow
and had a space leak, when the last chunk shall be handled correctly.
-}
newtype Builder a =
   Builder {run :: forall s.
      ChunkSize ->
      (Buffer s a -> ST s [SV.Vector a]) ->
      (Buffer s a -> ST s [SV.Vector a])
   }

type Buffer s a = (STV.Vector s a, Int)


-- instance Monoid (Builder a) where
{-
Storable constraint not needed in the current implementation,
but who knows what will be in future ...
-}
instance Storable a => Semigroup (Builder a) where
   {-# INLINE (<>) #-}
   x <> y = Builder (\cs -> run x cs . run y cs)

instance Storable a => Monoid (Builder a) where
   {-# INLINE mempty #-}
   {-# INLINE mappend #-}
   mempty = Builder (\_ -> id)
   mappend = (<>)


{- |
> toLazyStorableVector (ChunkSize 7) $ foldMap put ['a'..'z']
-}
{-# INLINE toLazyStorableVector #-}
toLazyStorableVector :: Storable a =>
   ChunkSize -> Builder a -> SVL.Vector a
toLazyStorableVector cs bld =
   SVL.fromChunks $
   runST (run bld cs (fmap (:[]) . fixVector) =<< newChunk cs)


{-# INLINE put #-}
put :: Storable a => a -> Builder a
put a =
   Builder (\cs cont (v0,i0) ->
      do STV.unsafeWrite v0 i0 a
         let i1 = succ i0
         if i1 < STV.length v0
           then
             cont (v0, i1)
           else
             liftM2 (:)
                -- we could call 'flush' here, but this requires an extra 'SV.take'
                (STV.unsafeFreeze v0)
                (Unsafe.interleaveST $
                 cont =<< newChunk cs)
   )

{-
put :: Storable a => a -> Builder a
put a =
   Builder (\cs cont (v0,i0) ->
      if i0 < STV.length v0
        then
          do STV.write v0 i0 a
             cont (v0, succ i0)
        else
          liftM2 (:)
             -- we could call 'flush' here, but this requires an extra 'SV.take'
             (STV.unsafeFreeze v0)
             (Unsafe.interleaveST $
              do (v1,i1) <- newChunk cs
                 STV.write v1 i1 a
                 cont (v1, succ i1))
   )
-}

{-
          lazyToStrictST $
          liftM2 (:)
             -- we could call 'flush' here, but this requires an extra 'SV.take'
             (STVL.unsafeFreeze v0)
             (strictToLazyST $
              do (v1,i1) <- newChunk cs
                 STV.write v1 i1 a
                 cont (v1, succ i1))
-}

{-
Prelude Control.Monad.ST.Lazy> Control.Monad.ST.runST (lazyToStrictST $ Monad.liftM2 (,) (strictToLazyST $ return 'a') (strictToLazyST (undefined::Monad m => m Char)))
*** Exception: Prelude.undefined
-}

{- |
Set a laziness break.
-}
{-# INLINE flush #-}
flush :: Storable a => Builder a
flush =
   Builder (\cs cont vi0 ->
      liftM2 (:)
         (fixVector vi0)
         (Unsafe.interleaveST $ cont =<< newChunk cs)
{-
      lazyToStrictST $
      liftM2 (:)
         (strictToLazyST $ fixVector vi0)
         (strictToLazyST $ cont =<< newChunk cs)
-}
   )

{-# INLINE newChunk #-}
newChunk :: (Storable a) =>
   ChunkSize -> ST s (Buffer s a)
newChunk (SVL.ChunkSize size) =
   fmap (flip (,) 0) $ STV.new_ size

{-# INLINE fixVector #-}
fixVector :: (Storable a) =>
   Buffer s a -> ST s (SV.Vector a)
fixVector ~(v1,i1) =
   fmap (SV.take i1) $ STV.unsafeFreeze v1
