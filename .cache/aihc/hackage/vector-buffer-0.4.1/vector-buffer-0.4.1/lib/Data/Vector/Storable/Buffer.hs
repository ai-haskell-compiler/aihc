{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Storable.Buffer
-- Copyright   :  (c) A.V.H. McPhail 2011
-- License     :  BSD3
--
-- Maintainer  :  Vivian McPhail <haskell.vivian.mcphail@gmail.com>
-- Stability   :  provisional
--
-- A buffer that can be used as a vector
-----------------------------------------------------------------------------

module Data.Vector.Storable.Buffer (
    Buffer,
    newBuffer,
    pushNextElement,
    toVector,
    mapBufferM, mapBufferM_,
) where

import Data.IORef

import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M

import Foreign hiding(new)

import Control.Monad.ST(RealWorld)

import Control.DeepSeq

import System.IO.Unsafe(unsafePerformIO)

-------------------------------------------------------------------

data Buffer a = B { nxt :: {-# UNPACK #-} !(IORef Int)    -- ^ next position to fill
                  , siz :: {-# UNPACK #-} !Int            -- ^ size  
                  , dat :: {-# UNPACK #-} !(M.IOVector a) -- ^ the data
                  }

instance NFData a => NFData (Buffer a) where
    rnf (B nxt siz dat) = rnf dat

-- now defined in Data.Vector.Storable.Mutable
--instance NFData a => NFData (M.MVector RealWorld a) where 

-- now defined in Data.Vector.Storable
--instance NFData a => NFData (V.Vector a) where

-- | create a new buffer
newBuffer :: Storable a 
            => Int       -- ^ Size
          -> IO (Buffer a)
newBuffer n = do
  v <- M.new n
  o <- newIORef 0
  return $ B o n v
{-# INLINE newBuffer #-}

-- | add the next element to the buffer
pushNextElement :: Storable a => Buffer a -> a -> IO ()
pushNextElement (B o n v) e = do
  i <- readIORef o
  M.unsafeWrite v i e
  if i == (n-1)
     then writeIORef o 0
     else writeIORef o (i+1)
{-# INLINE pushNextElement #-}

-- | convert to a vector
toVector :: (NFData a, Storable a) => Buffer a -> V.Vector a
toVector (B o n v) = unsafePerformIO $ do
   w <- M.new n
   i <- readIORef o
   M.unsafeWith v $ \p ->
       M.unsafeWith w $ \q -> do
         let n' = n-i
         copyArray q (p `advancePtr` i) n'
         if i /= 0
            then copyArray (q `advancePtr` n') p i
            else return ()
   r <- w `deepseq` V.unsafeFreeze w
   r `deepseq` return r
{-# INLINE toVector #-}

-- | monadic map over a buffer
mapBufferM :: (Storable a, Storable b) => (a -> IO b) -> Buffer a -> IO (V.Vector b)
mapBufferM f (B o n v) = do
  w <- M.new n
  i <- readIORef o
  go w 0 i n
  V.unsafeFreeze w
     where go w' !i' !o' !n' 
              | i' + 1 == n' = do
                         let j = i'+o'
                         x <- M.unsafeRead v (j `mod` n')
                         y <- f x
                         M.unsafeWrite w' i' y
              | otherwise         = do
                         let j = i'+o'
                         x <- M.unsafeRead v (j `mod` n')
                         y <- f x
                         M.unsafeWrite w' i' y
                         go w' ((i'+1) `mod` n') o' n'
{-# INLINE mapBufferM #-}

-- | monadic map over a buffer
mapBufferM_ :: (Storable a) => (a -> IO b) -> Buffer a -> IO ()
mapBufferM_ f (B o n v) = do
  i <- readIORef o
  go 0 i n
     where go !i' !o' !n' 
              | i' + 1 == n' = do
                         let j = i'+o'
                         x <- M.unsafeRead v (j `mod` n')
                         _ <- f x
                         return ()
              | otherwise         = do
                         let j = i'+o'
                         x <- M.unsafeRead v (j `mod` n')
                         _ <- f x
                         go ((i'+1) `mod` n') o' n'
{-# INLINE mapBufferM_ #-}
