-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.MissingUtils
-- Copyright   :  (c) Jason Dagit 2001
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  dagitj@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for primitive marshaling
--
-----------------------------------------------------------------------------
module Foreign.Marshal.MissingUtils (
  -- * General marshalling utilities
  -- ** Haskellish interface to memcpy and memove
  -- | (argument order: destination, source)
  copy,  -- :: Storable a => Ptr a -> Ptr a -> IO ()
  move   -- :: Storable a => Ptr a -> Ptr a -> IO ()
) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils

-- Haskellish interface to memcpy and memmove
-- ------------------------------------------

-- |Uses 'sizeOf' to copy bytes from the second area (source) into the
-- first (destination); the copied areas may /not/ overlap
--
{-# INLINE copy #-}
copy :: Storable a => Ptr a -> Ptr a -> IO ()
copy dest src = copyBytes dest src (sizeOf (type_ src))
  where
  type_ :: Ptr a -> a
  type_ = undefined

-- |Uses 'sizeOf' to copy bytes from the second area (source) into the
-- first (destination); the copied areas /may/ overlap
--
{-# INLINE move #-}
move :: Storable a => Ptr a -> Ptr a -> IO ()
move dest src = moveBytes dest src (sizeOf (type_ src))
  where
  type_ :: Ptr a -> a
  type_ = undefined
