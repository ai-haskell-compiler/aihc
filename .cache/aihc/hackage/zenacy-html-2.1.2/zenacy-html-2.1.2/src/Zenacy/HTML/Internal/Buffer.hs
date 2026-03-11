{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines an buffer type.
module Zenacy.HTML.Internal.Buffer
  ( Buffer(..)
  , bufferNew
  , bufferCapacity
  , bufferSize
  , bufferReset
  , bufferAppend
  , bufferApply
  , bufferTake
  , bufferContains
  , bufferPack
  , bufferString
  ) where

import Zenacy.HTML.Internal.BS
-- import Foreign
--   ( castPtr
--   , withForeignPtr
--   )
import Control.Monad.ST
  ( ST
  )
import Data.STRef
  ( STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  )
import qualified Data.DList as D
  ( empty
  , snoc
  , toList
  )
-- import Data.Vector.Storable.Mutable
import qualified Data.Vector.Unboxed as U
  ( freeze
  , slice
  , toList
  )
import Data.Vector.Unboxed.Mutable
  ( MVector
  )
import qualified Data.Vector.Unboxed.Mutable as U
-- import qualified Data.Vector.Storable.Mutable as U
  ( new
  , length
  , read
  , write
  , grow
  -- , unsafeToForeignPtr0
  )
import Data.Word
  ( Word8
  )
-- import System.IO.Unsafe
--   ( unsafePerformIO
--   )

-- | A type of buffer used to hold bytes.
data Buffer s = Buffer
  { bfCntl :: MVector s Int
  , bfData :: MVector s Word8
  }

-- | Makes a new buffer.
bufferNew :: ST s (STRef s (Buffer s))
bufferNew = do
  c <- U.new 1
  d <- U.new 100
  r <- newSTRef (Buffer c d)
  bufferReset r
  pure r

-- | Gets the capacity of the buffer.
bufferCapacity :: STRef s (Buffer s) -> ST s (Int, Int)
bufferCapacity r = do
  Buffer{..} <- readSTRef r
  pure (U.length bfCntl, U.length bfData)

-- | Gets the size of the buffer.
bufferSize :: STRef s (Buffer s) -> ST s Int
bufferSize r = do
  Buffer{..} <- readSTRef r
  U.read bfCntl 0

-- | Resets a buffer.
bufferReset :: STRef s (Buffer s) -> ST s ()
bufferReset r = do
  Buffer{..} <- readSTRef r
  U.write bfCntl 0 0

-- | Appends a word to a buffer.
bufferAppend :: Word8 -> STRef s (Buffer s) -> ST s ()
bufferAppend word r = do
  Buffer{..} <- readSTRef r
  i <- U.read bfCntl 0
  d <- if i + 1 < U.length bfData
       then pure bfData
       else do
         v <- U.grow bfData $ U.length bfData
         writeSTRef r $ Buffer bfCntl v
         pure v
  U.write d i word
  U.write bfCntl 0 (i + 1)

-- | Applies an action to each word in the buffer.
bufferApply :: (Word8 -> ST s ()) -> STRef s (Buffer s) -> ST s ()
bufferApply f r = do
  Buffer{..} <- readSTRef r
  n <- U.read bfCntl 0
  let go i
        | i < n =
            U.read bfData i >>= f >> go (i + 1)
        | otherwise =
            pure ()
  go 0

-- | Takes elements from the front of the buffer.
bufferTake :: Int -> STRef s (Buffer s) -> ST s [Word8]
bufferTake x r = do
  Buffer{..} <- readSTRef r
  n <- min x <$> U.read bfCntl 0
  let go i y
        | i < n = do
            a <- U.read bfData i
            go (i + 1) $ D.snoc y a
        | otherwise =
            pure $ D.toList y
  go 0 D.empty

-- | Determines if a buffer has the specified contents.
bufferContains :: [Word8] -> STRef s (Buffer s) -> ST s Bool
bufferContains x r = do
  n <- bufferSize r
  if n /= length x
     then pure False
     else do
       a <- bufferTake n r
       pure $ x == a

-- | Packs a buffer into a byte string.
bufferPack :: STRef s (Buffer s) -> ST s BS
bufferPack r = do
  Buffer{..} <- readSTRef r
  n <- U.read bfCntl 0
  bufferString bfData n

-- | Converts a storable vector to a byte string.
bufferString :: MVector s Word8 -> Int -> ST s BS
bufferString v n =
  U.freeze v >>= pure . bsPack . U.toList . U.slice 0 n
  -- pure $ unsafePerformIO $ do
  --   let (f, _) = U.unsafeToForeignPtr0 v
  --   withForeignPtr f $ \p ->
  --     S.packCStringLen (castPtr p, n)
