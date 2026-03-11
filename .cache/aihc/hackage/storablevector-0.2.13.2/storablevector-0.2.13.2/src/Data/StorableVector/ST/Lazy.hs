{-# LANGUAGE Rank2Types #-}
{- |
Module      : Data.StorableVector.ST.Strict
License     : BSD-style
Maintainer  : haskell@henning-thielemann.de
Stability   : experimental
Portability : portable, requires ffi
Tested with : GHC 6.4.1

Interface for access to a mutable StorableVector.
-}
module Data.StorableVector.ST.Lazy (
        Vector,
        new,
        new_,
        read,
        write,
        modify,
        unsafeRead,
        unsafeWrite,
        unsafeModify,
        freeze,
        unsafeFreeze,
        thaw,
        VST.length,
        runSTVector,
        mapST,
        mapSTLazy,
        ) where

-- import qualified Data.StorableVector.Base as V
import qualified Data.StorableVector as VS
import qualified Data.StorableVector.Lazy as VL

import qualified Data.StorableVector.ST.Strict as VST

import Data.StorableVector.ST.Strict (Vector)


import qualified Control.Monad.ST.Lazy as ST
import Control.Monad.ST.Lazy (ST)

import Foreign.Storable         (Storable)

-- import Prelude (Int, ($), (+), return, const, )
import Prelude hiding (read, length, )



-- * access to mutable storable vector

{-# INLINE new #-}
new :: (Storable e) =>
   Int -> e -> ST s (Vector s e)
new n x = ST.strictToLazyST (VST.new n x)

{-# INLINE new_ #-}
new_ :: (Storable e) =>
   Int -> ST s (Vector s e)
new_ n  =  ST.strictToLazyST (VST.new_ n)

{- |
> Control.Monad.ST.runST (do arr <- new_ 10; Monad.zipWithM_ (write arr) [9,8..0] ['a'..]; read arr 3)
-}
{-# INLINE read #-}
read :: (Storable e) =>
   Vector s e -> Int -> ST s e
read xs n = ST.strictToLazyST (VST.read xs n)

{- |
> VS.unpack $ runSTVector (do arr <- new_ 10; Monad.zipWithM_ (write arr) [9,8..0] ['a'..]; return arr)
-}
{-# INLINE write #-}
write :: (Storable e) =>
   Vector s e -> Int -> e -> ST s ()
write xs n x = ST.strictToLazyST (VST.write xs n x)

{-# INLINE modify #-}
modify :: (Storable e) =>
   Vector s e -> Int -> (e -> e) -> ST s ()
modify xs n f = ST.strictToLazyST (VST.modify xs n f)


{-# INLINE unsafeRead #-}
unsafeRead :: (Storable e) =>
   Vector s e -> Int -> ST s e
unsafeRead xs n = ST.strictToLazyST (VST.unsafeRead xs n)

{-# INLINE unsafeWrite #-}
unsafeWrite :: (Storable e) =>
   Vector s e -> Int -> e -> ST s ()
unsafeWrite xs n x = ST.strictToLazyST (VST.unsafeWrite xs n x)

{-# INLINE unsafeModify #-}
unsafeModify :: (Storable e) =>
   Vector s e -> Int -> (e -> e) -> ST s ()
unsafeModify xs n f = ST.strictToLazyST (VST.unsafeModify xs n f)


{-# INLINE freeze #-}
freeze :: (Storable e) =>
   Vector s e -> ST s (VS.Vector e)
freeze xs = ST.strictToLazyST (VST.freeze xs)

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (Storable e) =>
   Vector s e -> ST s (VS.Vector e)
unsafeFreeze xs = ST.strictToLazyST (VST.unsafeFreeze xs)

{-# INLINE thaw #-}
thaw :: (Storable e) =>
   VS.Vector e -> ST s (Vector s e)
thaw xs = ST.strictToLazyST (VST.thaw xs)



{-# INLINE runSTVector #-}
runSTVector :: (Storable e) =>
   (forall s. ST s (Vector s e)) -> VS.Vector e
runSTVector m = VST.runSTVector (ST.lazyToStrictST m)



-- * operations on immutable storable vector within ST monad

{- |
> :module + Data.STRef
> VS.unpack $ Control.Monad.ST.runST (do ref <- newSTRef 'a'; mapST (\ _n -> do c <- readSTRef ref; modifySTRef ref succ; return c) (VS.pack [1,2,3,4::Data.Int.Int16]))
-}
{-# INLINE mapST #-}
mapST :: (Storable a, Storable b) =>
   (a -> ST s b) -> VS.Vector a -> ST s (VS.Vector b)
mapST f xs =
   ST.strictToLazyST (VST.mapST (ST.lazyToStrictST . f) xs)


{- |
> *Data.StorableVector.ST.Strict Data.STRef> VL.unpack $ Control.Monad.ST.runST (do ref <- newSTRef 'a'; mapSTLazy (\ _n -> do c <- readSTRef ref; modifySTRef ref succ; return c) (VL.pack VL.defaultChunkSize [1,2,3,4::Data.Int.Int16]))
> "abcd"

The following should not work on infinite streams,
since we are in 'ST' with strict '>>='.
But it works. Why?

> *Data.StorableVector.ST.Strict Data.STRef.Lazy> VL.unpack $ Control.Monad.ST.Lazy.runST (do ref <- newSTRef 'a'; mapSTLazy (\ _n -> do c <- readSTRef ref; modifySTRef ref succ; return c) (VL.pack VL.defaultChunkSize [0::Data.Int.Int16 ..]))
> "Interrupted.
-}
{-# INLINE mapSTLazy #-}
mapSTLazy :: (Storable a, Storable b) =>
   (a -> ST s b) -> VL.Vector a -> ST s (VL.Vector b)
mapSTLazy f (VL.SV xs) =
   fmap VL.SV $ mapM (mapST f) xs
