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
module Data.StorableVector.ST.Strict (
        Vector,
        new,
        new_,
        read,
        write,
        modify,
        maybeRead,
        maybeWrite,
        maybeModify,
        unsafeRead,
        unsafeWrite,
        unsafeModify,
        freeze,
        unsafeFreeze,
        thaw,
        length,
        runSTVector,
        mapST,
        mapSTLazy,
        ) where

import Data.StorableVector.ST.Private
          (Vector(SV), create, unsafeCreate, unsafeToVector, )
import qualified Data.StorableVector.Base as V
import qualified Data.StorableVector as VS
import qualified Data.StorableVector.Lazy as VL

import Control.Monad.ST.Strict (ST, runST, )

import Foreign.Ptr              (Ptr, )
import Foreign.ForeignPtr       (withForeignPtr, )
import Foreign.Storable         (Storable(peek, poke))
import Foreign.Marshal.Array    (advancePtr, copyArray, )
import qualified System.Unsafe as Unsafe

import qualified Data.Traversable as Traversable
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (isJust, )

-- import Prelude (Int, ($), (+), return, const, )
import Prelude hiding (read, length, )


-- * access to mutable storable vector

{-# INLINE new #-}
new :: (Storable e) =>
   Int -> e -> ST s (Vector s e)
new n x =
   unsafeCreate n $
   let {-# INLINE go #-}
       go m p =
         if m>0
           then poke p x >> go (pred m) (V.incPtr p)
           else return ()
   in  go n

{-# INLINE new_ #-}
new_ :: (Storable e) =>
   Int -> ST s (Vector s e)
new_ n =
   unsafeCreate n (const (return ()))


{- |
> Control.Monad.ST.runST (do arr <- new_ 10; Monad.zipWithM_ (write arr) [9,8..0] ['a'..]; read arr 3)
-}
{-# INLINE read #-}
read :: (Storable e) =>
   Vector s e -> Int -> ST s e
read v n =
   access "read" v n $ unsafeRead v n

{- |
> VS.unpack $ runSTVector (do arr <- new_ 10; Monad.zipWithM_ (write arr) [9,8..0] ['a'..]; return arr)
-}
{-# INLINE write #-}
write :: (Storable e) =>
   Vector s e -> Int -> e -> ST s ()
write v n x =
   access "write" v n $ unsafeWrite v n x

{- |
> VS.unpack $ runSTVector (do arr <- new 10 'a'; Monad.mapM_ (\n -> modify arr (mod n 8) succ) [0..10]; return arr)
-}
{-# INLINE modify #-}
modify :: (Storable e) =>
   Vector s e -> Int -> (e -> e) -> ST s ()
modify v n f =
   access "modify" v n $ unsafeModify v n f

{-# INLINE access #-}
access :: (Storable e) =>
   String -> Vector s e -> Int -> ST s a -> ST s a
access name (SV _v l) n act =
   if 0<=n && n<l
     then act
     else error ("StorableVector.ST." ++ name ++ ": index out of range")


{- |
Returns @Just e@, when the element @e@ could be read
and 'Nothing' if the index was out of range.
This way you can avoid duplicate index checks
that may be needed when using 'read'.

> Control.Monad.ST.runST (do arr <- new_ 10; Monad.zipWithM_ (write arr) [9,8..0] ['a'..]; read arr 3)

In future 'maybeRead' will replace 'read'.
-}
{-# INLINE maybeRead #-}
maybeRead :: (Storable e) =>
   Vector s e -> Int -> ST s (Maybe e)
maybeRead v n =
   maybeAccess v n $ unsafeRead v n

{- |
Returns 'True' if the element could be written
and 'False' if the index was out of range.

> runSTVector (do arr <- new_ 10; foldr (\c go i -> maybeWrite arr i c >>= \cont -> if cont then go (succ i) else return arr) (error "unreachable") ['a'..] 0)

In future 'maybeWrite' will replace 'write'.
-}
{-# INLINE maybeWrite #-}
maybeWrite :: (Storable e) =>
   Vector s e -> Int -> e -> ST s Bool
maybeWrite v n x =
   fmap isJust $ maybeAccess v n $ unsafeWrite v n x

{- |
Similar to 'maybeWrite'.

In future 'maybeModify' will replace 'modify'.
-}
{-# INLINE maybeModify #-}
maybeModify :: (Storable e) =>
   Vector s e -> Int -> (e -> e) -> ST s Bool
maybeModify v n f =
   fmap isJust $ maybeAccess v n $ unsafeModify v n f

{-# INLINE maybeAccess #-}
maybeAccess :: (Storable e) =>
   Vector s e -> Int -> ST s a -> ST s (Maybe a)
maybeAccess (SV _v l) n act =
   Traversable.sequence $ toMaybe (0<=n && n<l) act
{-
   if 0<=n && n<l
     then fmap Just act
     else return Nothing
-}

{-# INLINE unsafeRead #-}
unsafeRead :: (Storable e) =>
   Vector s e -> Int -> ST s e
unsafeRead v n =
   unsafeAccess v n $ peek

{-# INLINE unsafeWrite #-}
unsafeWrite :: (Storable e) =>
   Vector s e -> Int -> e -> ST s ()
unsafeWrite v n x =
   unsafeAccess v n $ \p -> poke p x

{-# INLINE unsafeModify #-}
unsafeModify :: (Storable e) =>
   Vector s e -> Int -> (e -> e) -> ST s ()
unsafeModify v n f =
   unsafeAccess v n $ \p -> poke p . f =<< peek p

{-# INLINE unsafeAccess #-}
unsafeAccess :: (Storable e) =>
   Vector s e -> Int -> (Ptr e -> IO a) -> ST s a
unsafeAccess (SV v _l) n act =
   Unsafe.ioToST (withForeignPtr v $ \p -> act (advancePtr p n))


{-# INLINE freeze #-}
freeze :: (Storable e) =>
   Vector s e -> ST s (VS.Vector e)
freeze (SV x l) =
   Unsafe.ioToST $
   V.create l $ \p ->
   withForeignPtr x $ \f ->
   copyArray p f (fromIntegral l)

{- |
This is like 'freeze' but it does not copy the vector.
You must make sure that you never write again to the array.
It is best to use 'unsafeFreeze' only at the end of a block,
that is run by 'runST'.
-}
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (Storable e) =>
   Vector s e -> ST s (VS.Vector e)
unsafeFreeze = unsafeToVector


{-# INLINE thaw #-}
thaw :: (Storable e) =>
   VS.Vector e -> ST s (Vector s e)
thaw v =
   Unsafe.ioToST $
   V.withStartPtr v $ \f l ->
   create l $ \p ->
   copyArray p f (fromIntegral l)


{-# INLINE length #-}
length ::
   Vector s e -> Int
length (SV _v l) = l


{-# INLINE runSTVector #-}
runSTVector :: (Storable e) =>
   (forall s. ST s (Vector s e)) -> VS.Vector e
runSTVector m =
   runST (unsafeToVector =<< m)



-- * operations on immutable storable vector within ST monad

{- |
> :module + Data.STRef
> VS.unpack $ Control.Monad.ST.runST (do ref <- newSTRef 'a'; mapST (\ _n -> do c <- readSTRef ref; modifySTRef ref succ; return c) (VS.pack [1,2,3,4::Data.Int.Int16]))
-}
{-# INLINE mapST #-}
mapST :: (Storable a, Storable b) =>
   (a -> ST s b) -> VS.Vector a -> ST s (VS.Vector b)
mapST f (V.SV px sx n) =
   let {-# INLINE go #-}
       go l q p =
          if l>0
            then
               do Unsafe.ioToST . poke p =<< f =<< Unsafe.ioToST (peek q)
                  go (pred l) (advancePtr q 1) (advancePtr p 1)
            else return ()
   in  do ys@(SV py _) <- new_ n
          go n
              (Unsafe.foreignPtrToPtr px `advancePtr` sx)
              (Unsafe.foreignPtrToPtr py)
          unsafeToVector ys

{-
mapST f xs@(V.SV v s l) =
   let go l q p =
          if l>0
            then
               do poke p =<< stToIO . f =<< peek q
                  go (pred l) (advancePtr q 1) (advancePtr p 1)
            else return ()
       n = VS.length xs
   in  return $ V.unsafeCreate n $ \p ->
          withForeignPtr v $ \q -> go n (advancePtr q s) p
-}


{- |
> *Data.StorableVector.ST.Strict Data.STRef> VL.unpack $ Control.Monad.ST.runST (do ref <- newSTRef 'a'; mapSTLazy (\ _n -> do c <- readSTRef ref; modifySTRef ref succ; return c) (VL.pack VL.defaultChunkSize [1,2,3,4::Data.Int.Int16]))
> "abcd"

The following should not work on infinite streams,
since we are in 'ST' with strict '>>='.
But it works. Why?

> *Data.StorableVector.ST.Strict Data.STRef> VL.unpack $ Control.Monad.ST.runST (do ref <- newSTRef 'a'; mapSTLazy (\ _n -> do c <- readSTRef ref; modifySTRef ref succ; return c) (VL.pack VL.defaultChunkSize [0::Data.Int.Int16 ..]))
> "Interrupted.
-}
{-# INLINE mapSTLazy #-}
mapSTLazy :: (Storable a, Storable b) =>
   (a -> ST s b) -> VL.Vector a -> ST s (VL.Vector b)
mapSTLazy f (VL.SV xs) =
   fmap VL.SV $ mapM (mapST f) xs
