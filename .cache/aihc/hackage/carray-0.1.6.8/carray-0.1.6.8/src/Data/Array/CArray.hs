-- |
-- Module      : Data.Array.CArray
-- Copyright   : (c) 2008 Jed Brown
-- License     : BSD-style
-- 
-- Maintainer  : jed@59A2.org
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides the immutable 'CArray' which uses pinned memory on the
-- GC'd heap.  Elements are stored according to the class 'Storable'.  You can
-- obtain a pointer to the array contents to manipulate elements from
-- languages like C.
--
-- 'CArray' is 16-byte aligned by default.  If you create a 'CArray' with
-- 'unsafeForeignPtrToCArray' then it may not be aligned.  This will be an issue
-- if you intend to use SIMD instructions.
--
-- 'CArray' is similar to 'Data.Array.Unboxed.UArray' but slower if you stay
-- within Haskell.  'CArray' can handle more types and can be used by external
-- libraries.
--
-- 'CArray' has an instance of 'Binary'.
-----------------------------------------------------------------------------

module Data.Array.CArray (
    -- * CArray type
    CArray,

    -- * Multi-dimensional

    -- ** Fast reshaping
    reshape,
    flatten,

    -- ** Query
    rank,
    shape,
    size,

    -- * Mapping

    -- ** General
    ixmapWithIndP,
    ixmapWithInd,
    ixmapWithP,
    ixmapWith,
    ixmapP,

    -- ** Slicing
    sliceStrideWithP,
    sliceStrideWith,
    sliceStrideP,
    sliceStride,
    sliceWithP,
    sliceWith,
    sliceP,
    slice,

    -- * Lifting
    liftArrayP,
    liftArray,
    liftArray2P,
    liftArray2,
    liftArray3P,
    liftArray3,

    -- * Norms
    normp,
    norm2,
    normSup,

    -- * Types
    Shapable,
    Abs,

    -- * Unsafe low-level
    withCArray,
    unsafeForeignPtrToCArray,
    toForeignPtr,
    unsafeCArrayToByteString,
    unsafeByteStringToCArray,
    createCArray,

   -- * The overloaded immutable array interface
   module Data.Array.IArray
) where

import Data.Ix.Shapable
import Data.Array.IArray
import Data.Array.CArray.Base
