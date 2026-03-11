{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.Writer.Storable where

import Control.Monad.ST
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.STRef                   as ST
import qualified Data.Vector.Storable.Mutable as DVSM

{- HLINT ignore "Reduce duplication"  -}

data Writer s = Writer
  { vector   :: DVSM.MVector s Word64
  , position :: ST.STRef s Int
  }

full :: Writer s -> ST s Bool
full writer = do
  p <- ST.readSTRef $ position writer
  return $ p * 64 >= DVSM.length (vector writer)
{-# INLINE full #-}

newWriter :: Int -> ST s (Writer s)
newWriter size = do
  v <- DVSM.new size
  p <- ST.newSTRef 0
  return $ Writer v p
{-# INLINE newWriter #-}

unsafeWriteBit :: Writer s -> Word64 -> ST s ()
unsafeWriteBit writer w = do
  let v = vector   writer             -- vector
  p <- ST.readSTRef $ position writer -- position
  let i = p .>. 6                     -- index into vector
  let o = p .&. 0x3f                  -- offset within a word
  e <- DVSM.unsafeRead v i
  DVSM.unsafeWrite v i (((w .&. 1) .<. fromIntegral o) .|. e)
  ST.writeSTRef (position writer) (p + 1)
{-# INLINE unsafeWriteBit #-}

unsafeWriteLoBits :: Writer s -> Int -> Word64 -> ST s ()
unsafeWriteLoBits writer c w = do
  let u = w .&. ((1 .<. fromIntegral c) - 1)
  let v = vector   writer             -- vector
  p <- ST.readSTRef $ position writer -- position
  let i = p .>. 6                     -- index into vector
  let o = p .&. 0x3f                  -- offset within a word
  lo <- DVSM.unsafeRead v i
  DVSM.unsafeWrite v i $ lo .|. (u .<. fromIntegral o)
  ST.writeSTRef (position writer) (p + c)
{-# INLINE unsafeWriteLoBits #-}

unsafeWriteBits :: Writer s -> Int -> Word64 -> ST s ()
unsafeWriteBits writer c w = do
  let u = w .&. ((1 .<. fromIntegral c) - 1)  -- masked word
  let v = vector writer                       -- vector
  p <- ST.readSTRef $ position writer         -- position
  let i = p .>. 6                             -- index into vector for lo part
  let j = i + 1                               -- index into vector for hi part
  let o = p .&. 0x3f                          -- offset within a word
  lo <- DVSM.unsafeRead v i
  DVSM.unsafeWrite v i $ lo .|. (u .<. fromIntegral o)
  hi <- DVSM.unsafeRead v j
  DVSM.unsafeWrite v j $ hi .|. (u .>. fromIntegral (64 - o))
  ST.writeSTRef (position writer) (p + c)
{-# INLINE unsafeWriteBits #-}

written :: Writer s -> ST s (DVSM.MVector s Word64)
written writer = do
  p <- ST.readSTRef $ position writer         -- position
  return $ DVSM.take ((p + 63) `div` 64) (vector writer)
{-# INLINE written #-}
