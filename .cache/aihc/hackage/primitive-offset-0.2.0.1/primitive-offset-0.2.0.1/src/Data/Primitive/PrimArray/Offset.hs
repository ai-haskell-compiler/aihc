{-# LANGUAGE DuplicateRecordFields #-}

{- | Data types for describing an array paired with an index
into it. This is intended to be used in wrappers for @unsafe@
FFI calls. For example, the POSIX function @recvfrom@ takes
a @socklen_t*@ argument. (Let us assume that @socklen_t@
is equivalant to @int@ for this example.) How is this argument best
described by a Haskell type? When working with pinned memory,
the best option @'Ptr' 'CInt'@.
It works equally well regardless of whether we originally had an
array of 'CInt' or a pointer to a single 'CInt'. This works because
of functions like 'Data.Primitive.advancePtr' and
'Foreign.Ptr.plusPtr' that effectively index into an array.
Unpinned memory, however, is trickier. We want to have the full
flexibility (handling both a single-element or
multi-element buffer) that @'Ptr' 'CInt'@ affords. We cannot
offset into a 'MutablePrimArray' to get a new one like we
could with 'Ptr'. (Such a function is not possible because
unpinned memory can be relocated.) So, the offseting must
be done in the C function wrapped by the @unsafe@ FFI. This
means that the offset must be passed together with the
'MutablePrimArray'. This is the precisely the product that
'MutablePrimArrayOffset' represents. In a type signature, it
provides additional clarity about the meaning of the offset.

This library is used in the extensively in the @posix-api@
library to clarify intent in a number of type signatures.
-}
module Data.Primitive.PrimArray.Offset
  ( -- * Types
    PrimArrayOffset (..)
  , MutablePrimArrayOffset (..)

    -- * Resolution
  , indexOffset
  , readOffset
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive (MutablePrimArray, Prim, PrimArray, indexPrimArray, readPrimArray)

-- | A primitive array and an index into the array.
data PrimArrayOffset a = PrimArrayOffset
  { array :: {-# UNPACK #-} !(PrimArray a)
  , offset :: {-# UNPACK #-} !Int
  }

-- | A mutable primitive array and an index into the array.
data MutablePrimArrayOffset s a = MutablePrimArrayOffset
  { array :: {-# UNPACK #-} !(MutablePrimArray s a)
  , offset :: {-# UNPACK #-} !Int
  }

-- | Recover the element in the primitive array.
indexOffset ::
  (Prim a) =>
  PrimArrayOffset a ->
  a
{-# INLINE indexOffset #-}
indexOffset (PrimArrayOffset arr ix) = indexPrimArray arr ix

-- | Recover the element in the mutable primitive array.
readOffset ::
  (PrimMonad m, Prim a) =>
  MutablePrimArrayOffset (PrimState m) a ->
  m a
{-# INLINE readOffset #-}
readOffset (MutablePrimArrayOffset arr ix) = readPrimArray arr ix
