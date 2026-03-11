{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Unboxed.Buffer
-- Copyright   :  (c) A.V.H. McPhail 2011
-- License     :  BSD3
--
-- Maintainer  :  Vivian McPhail <haskell.vivian.mcphail@gmail.com>
-- Stability   :  provisional
--
-- A buffer that can be used as a vector
-----------------------------------------------------------------------------

module Data.Vector.Unboxed.Buffer (
    Buffer,
    newBuffer,
    pushNextElement,
    toVector,
    mapBufferM, mapBufferM_,
) where

import Data.IORef

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

import Foreign hiding(new)

-------------------------------------------------------------------

data Buffer a = B { nxt :: {-# UNPACK #-} !(IORef Int)    -- ^ next position to fill
                  , siz :: {-# UNPACK #-} !Int            -- ^ size  
                  , dat ::                !(M.IOVector a) -- ^ the data
                  }

-- | create a new buffer
newBuffer :: M.Unbox a 
            => Int       -- ^ Size
          -> IO (Buffer a)
newBuffer n = do
  v <- M.new n
  o <- newIORef 0
  return $ B o n v
{-# INLINE newBuffer #-}

-- | add the next element to the buffer
pushNextElement :: M.Unbox a => Buffer a -> a -> IO ()
pushNextElement (B o n v) e = do
  i <- readIORef o
  M.unsafeWrite v i e
  if i == (n-1)
     then writeIORef o 0
     else writeIORef o (i+1)
{-# INLINE pushNextElement #-}

-- | convert to a vector
toVector :: M.Unbox a => Buffer a -> IO (V.Vector a)
toVector (B o n v) = do
   w <- M.new n
   i <- readIORef o
   let n' = n-i
   let vs = M.slice i  n' v
   let ws = M.slice 0  n' w
   M.unsafeCopy ws vs
   if i /= 0
      then do
        let vf = M.slice 0  i  v
        let wf = M.slice n' i  w
        M.unsafeCopy wf vf
      else return ()
   V.unsafeFreeze w
{-# INLINE toVector #-}

-- | monadic map over a buffer
mapBufferM :: (M.Unbox a, M.Unbox b) => (a -> IO b) -> Buffer a -> IO (V.Vector b)
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
mapBufferM_ :: (M.Unbox a) => (a -> IO b) -> Buffer a -> IO ()
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
