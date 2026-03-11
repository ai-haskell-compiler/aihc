-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.IOCArray
-- Copyright   : (c) 2008 Jed Brown
-- License     : BSD-style
-- 
-- Maintainer  : jed@59A2.org
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides both the mutable 'IOCArray' which uses pinned memory on
-- the GC'd heap.  Elements are stored according to the class 'Storable'.  You
-- can obtain a pointer to the array contents to manipulate elements from
-- languages like C.
--
-- 'IOCArray' is 16-byte aligned by default.  If you create a 'IOCArray' with
-- 'unsafeForeignPtrToIOCArray' then it may not be aligned.  This will be an
-- issue if you intend to use SIMD instructions.
--
-- 'IOCArray' is equivalent to 'Data.Array.Storable.StorableArray' and similar
-- to 'Data.Array.IO.IOUArray' but slower.  'IOCArray' has O(1) versions of
-- 'unsafeFreeze' and 'unsafeThaw' when converting to/from 'CArray'.
-----------------------------------------------------------------------------


module Data.Array.IOCArray (
    -- * IOCArray type
    IOCArray,

    -- * Foreign support
    withIOCArray,
    touchIOCArray,
    unsafeForeignPtrToIOCArray,

    -- * The overloaded mutable array interface
    module Data.Array.MArray
) where

import Data.Array.CArray.Base
import Data.Array.MArray
