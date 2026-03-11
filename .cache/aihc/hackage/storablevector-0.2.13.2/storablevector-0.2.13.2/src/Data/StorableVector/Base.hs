{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
--
-- Module      : Data.StorableVector.Base
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable, requires ffi and cpp
-- Tested with : GHC 6.4.1 and Hugs March 2005
-- 

-- | A module containing semi-public StorableVector internals. This exposes
-- the StorableVector representation and low level construction functions.
-- Modules which extend the StorableVector system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Data.StorableVector.Base (

        -- * The @Vector@ type and representation
        Vector(..),             -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Unchecked access
        unsafeHead,             -- :: Vector a -> a
        unsafeTail,             -- :: Vector a -> Vector a
        unsafeLast,             -- :: Vector a -> a
        unsafeInit,             -- :: Vector a -> Vector a
        unsafeIndex,            -- :: Vector a -> Int -> a
        unsafeTake,             -- :: Int -> Vector a -> Vector a
        unsafeDrop,             -- :: Int -> Vector a -> Vector a

        -- * Low level introduction and elimination
        create,                 -- :: Int -> (Ptr a -> IO ()) -> IO (Vector a)
        createAndTrim,          -- :: Int -> (Ptr a -> IO Int) -> IO (Vector a)
        createAndTrim',         -- :: Int -> (Ptr a -> IO (Int, Int, b)) -> IO (Vector a, b)

        unsafeCreate,           -- :: Int -> (Ptr a -> IO ()) ->  Vector a

        fromForeignPtr,         -- :: ForeignPtr a -> Int -> Vector a
        toForeignPtr,           -- :: Vector a -> (ForeignPtr a, Int, Int)
        withStartPtr,           -- :: Vector a -> (Ptr a -> Int -> IO b) -> IO b
        incPtr,                 -- :: Ptr a -> Ptr a

        inlinePerformIO

  ) where

import Foreign.Ptr              (Ptr)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr, )
import Foreign.Marshal.Array    (advancePtr, copyArray)
import Foreign.Storable         (Storable(peekElemOff))

import Data.StorableVector.Memory (mallocForeignPtrArray, )

import Control.DeepSeq          (NFData, rnf)
import Control.Exception        (assert)

#if defined(__GLASGOW_HASKELL__)
import Data.Generics            (Data, Typeable)
import GHC.Base                 (realWorld#)
import GHC.IO                   (IO(IO), )
#endif

import qualified System.Unsafe as Unsafe

-- CFILES stuff is Hugs only
{-# CFILES cbits/fpstring.c #-}

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a vector, supporting many efficient
-- operations.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
data Vector a = SV {-# UNPACK #-} !(ForeignPtr a)
                   {-# UNPACK #-} !Int                -- offset
                   {-# UNPACK #-} !Int                -- length
#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif


instance (Storable a) => NFData (Vector a) where
    rnf (SV _ _ _) = ()


-- ---------------------------------------------------------------------
--
-- Extensions to the basic interface
--

-- | A variety of 'head' for non-empty Vectors. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the Vector is non-empty.
unsafeHead :: (Storable a) => Vector a -> a
unsafeHead (SV x s l) = assert (l > 0) $
    inlinePerformIO $ withForeignPtr x $ \p -> peekElemOff p s
{-# INLINE unsafeHead #-}

-- | A variety of 'tail' for non-empty Vectors. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the Vector is non-empty.
unsafeTail :: (Storable a) => Vector a -> Vector a
unsafeTail (SV ps s l) = assert (l > 0) $ SV ps (s+1) (l-1)
{-# INLINE unsafeTail #-}

-- | A variety of 'last' for non-empty Vectors. 'unsafeLast' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the Vector is non-empty.
unsafeLast :: (Storable a) => Vector a -> a
unsafeLast (SV x s l) = assert (l > 0) $
    inlinePerformIO $ withForeignPtr x $ \p -> peekElemOff p (s+l-1)
{-# INLINE unsafeLast #-}

-- | A variety of 'init' for non-empty Vectors. 'unsafeInit' omits the
-- check for the empty case. As with 'unsafeLast', the programmer must
-- provide a separate proof that the Vector is non-empty.
unsafeInit :: (Storable a) => Vector a -> Vector a
unsafeInit (SV ps s l) = assert (l > 0) $ SV ps s (l-1)
{-# INLINE unsafeInit #-}

-- | Unsafe 'Vector' index (subscript) operator, starting from 0, returning a
-- single element.  This omits the bounds check, which means there is an
-- accompanying obligation on the programmer to ensure the bounds are checked in
-- some other way.
unsafeIndex :: (Storable a) => Vector a -> Int -> a
unsafeIndex (SV x s l) i = assert (i >= 0 && i < l) $
    inlinePerformIO $ withForeignPtr x $ \p -> peekElemOff p (s+i)
{-# INLINE unsafeIndex #-}

-- | A variety of 'take' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeTake :: (Storable a) => Int -> Vector a -> Vector a
unsafeTake n (SV x s l) = assert (0 <= n && n <= l) $ SV x s n
{-# INLINE unsafeTake #-}

-- | A variety of 'drop' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeDrop :: (Storable a) => Int -> Vector a -> Vector a
unsafeDrop n (SV x s l) = assert (0 <= n && n <= l) $ SV x (s+n) (l-n)
{-# INLINE unsafeDrop #-}


instance (Storable a, Show a) => Show (Vector a) where
   showsPrec p xs@(SV _ _ l) =
      showParen (p>=10)
         (showString "Vector.pack " .
          showsPrec 10 (map (unsafeIndex xs) [0..(l-1)]))


-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(1)/ Build a Vector from a ForeignPtr
fromForeignPtr :: ForeignPtr a -> Int -> Vector a
fromForeignPtr fp l = SV fp 0 l

-- | /O(1)/ Deconstruct a ForeignPtr from a Vector
toForeignPtr :: Vector a -> (ForeignPtr a, Int, Int)
toForeignPtr (SV ps s l) = (ps, s, l)

-- | Run an action that is initialized
-- with a pointer to the first element to be used.
withStartPtr :: Storable a => Vector a -> (Ptr a -> Int -> IO b) -> IO b
withStartPtr (SV x s l) f =
   withForeignPtr x $ \p -> f (p `advancePtr` s) l
{-# INLINE withStartPtr #-}

incPtr :: (Storable a) => Ptr a -> Ptr a
incPtr v = advancePtr v 1
{-# INLINE incPtr #-}

-- | A way of creating Vectors outside the IO monad. The @Int@
-- argument gives the final size of the Vector. Unlike
-- 'createAndTrim' the Vector is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: (Storable a) => Int -> (Ptr a -> IO ()) -> Vector a
unsafeCreate l f = Unsafe.performIO (create l f)
{-# INLINE unsafeCreate #-}

-- | Wrapper of mallocForeignPtrArray.
create :: (Storable a) => Int -> (Ptr a -> IO ()) -> IO (Vector a)
create l f = do
    fp <- mallocForeignPtrArray l
    withForeignPtr fp $ \p -> f p
    return $! SV fp 0 l

-- | Given the maximum size needed and a function to make the contents
-- of a Vector, createAndTrim makes the 'Vector'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is realloced to this size.
--
-- createAndTrim is the main mechanism for creating custom, efficient
-- Vector functions, using Haskell or C functions to fill the space.
--
createAndTrim :: (Storable a) => Int -> (Ptr a -> IO Int) -> IO (Vector a)
createAndTrim l f = do
    fp <- mallocForeignPtrArray l
    withForeignPtr fp $ \p -> do
        l' <- f p
        if assert (l' <= l) $ l' >= l
            then return $! SV fp 0 l
            else create l' $ \p' -> copyArray p' p l'

createAndTrim' :: (Storable a) => Int 
                               -> (Ptr a -> IO (Int, Int, b))
                               -> IO (Vector a, b)
createAndTrim' l f = do
    fp <- mallocForeignPtrArray l
    withForeignPtr fp $ \p -> do
        (off, l', res) <- f p
        if assert (l' <= l) $ l' >= l
            then return $! (SV fp 0 l, res)
            else do ps <- create l' $ \p' -> copyArray p' (p `advancePtr` off) l'
                    return $! (ps, res)

-- | Just like Unsafe.performIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @Unsafe.performIO@.
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = Unsafe.performIO
#endif
