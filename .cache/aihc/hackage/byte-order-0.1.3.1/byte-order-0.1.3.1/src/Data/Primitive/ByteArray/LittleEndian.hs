{- | This is drop-in replacement for the read, write, and index functions
present in @Data.Primitive.ByteArray@ and @Data.Primitive.ByteArray.Unaligned@.
While the functions from those modules use native byte order, the functions
in this one use little-endian byte order (least significant byte first).
-}
module Data.Primitive.ByteArray.LittleEndian
  ( -- * Aligned
    writeByteArray
  , readByteArray
  , indexByteArray

    -- * Unaligned
  , writeUnalignedByteArray
  , readUnalignedByteArray
  , indexUnalignedByteArray
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive (ByteArray, MutableByteArray, Prim)
import qualified Data.Primitive as PM
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned)
import qualified Data.Primitive.ByteArray.Unaligned as PMU
import System.ByteOrder (Bytes, fromLittleEndian, toLittleEndian)

{- | Write a primitive value to the byte array. The offset is given
in elements of type @a@ rather than in bytes. The least significant
byte in the value comes first.
-}
writeByteArray ::
  (PrimMonad m, Prim a, Bytes a) =>
  MutableByteArray (PrimState m) ->
  Int ->
  a ->
  m ()
writeByteArray arr ix v = PM.writeByteArray arr ix (toLittleEndian v)

{- | Read a primitive value from the byte array, interpreting the first
byte as the least significant one. The offset is given in elements of
type @a@ rather than in bytes.
-}
readByteArray ::
  (PrimMonad m, Prim a, Bytes a) =>
  MutableByteArray (PrimState m) ->
  Int ->
  m a
readByteArray arr ix = fromLittleEndian <$> PM.readByteArray arr ix

{- | Read a primitive value from the byte array, interpreting the first
byte as the least significant one. The offset is given in elements of
type @a@ rather than in bytes.
-}
indexByteArray :: (Prim a, Bytes a) => ByteArray -> Int -> a
indexByteArray arr ix = fromLittleEndian (PM.indexByteArray arr ix)

{- | Write a primitive value to the byte array. The offset is given
in bytes rather than in elements of type @a@. The least significant
byte in the value comes first.
-}
writeUnalignedByteArray ::
  (PrimMonad m, PrimUnaligned a, Bytes a) =>
  MutableByteArray (PrimState m) ->
  Int ->
  a ->
  m ()
writeUnalignedByteArray arr ix v = PMU.writeUnalignedByteArray arr ix (toLittleEndian v)

{- | Read a primitive value from the byte array, interpreting the first
byte as the least significant one. The offset is given in bytes rather
than in elements of type @a@.
-}
readUnalignedByteArray ::
  (PrimMonad m, PrimUnaligned a, Bytes a) =>
  MutableByteArray (PrimState m) ->
  Int ->
  m a
readUnalignedByteArray arr ix = fromLittleEndian <$> PMU.readUnalignedByteArray arr ix

{- | Read a primitive value from the byte array, interpreting the first
byte as the least significant one. The offset is given in bytes rather
than in elements of type @a@.
-}
indexUnalignedByteArray ::
  (PrimUnaligned a, Bytes a) =>
  ByteArray ->
  Int ->
  a
indexUnalignedByteArray arr ix = fromLittleEndian (PMU.indexUnalignedByteArray arr ix)
