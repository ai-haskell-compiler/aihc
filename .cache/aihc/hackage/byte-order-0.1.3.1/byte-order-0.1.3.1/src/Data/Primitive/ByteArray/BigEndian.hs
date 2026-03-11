{- | This is drop-in replacement for the read, write, and index functions
present in @Data.Primitive.ByteArray@ and @Data.Primitive.ByteArray.Unaligned@.
While the functions from those modules use native byte order, the functions
in this one use big-endian byte order (most significant byte first).
-}
module Data.Primitive.ByteArray.BigEndian
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
import System.ByteOrder (Bytes, fromBigEndian, toBigEndian)

{- | Write a primitive value to the byte array. The offset is given
in elements of type @a@ rather than in bytes. The most significant
byte in the value comes first.
-}
writeByteArray :: (PrimMonad m, Prim a, Bytes a) => MutableByteArray (PrimState m) -> Int -> a -> m ()
writeByteArray arr ix v = PM.writeByteArray arr ix (toBigEndian v)

{- | Read a primitive value from the byte array, interpreting the first
byte as the most significant one. The offset is given in elements of
type @a@ rather than in bytes.
-}
readByteArray :: (PrimMonad m, Prim a, Bytes a) => MutableByteArray (PrimState m) -> Int -> m a
readByteArray arr ix = fromBigEndian <$> PM.readByteArray arr ix

{- | Read a primitive value from the byte array, interpreting the first
byte as the most significant one. The offset is given in elements of
type @a@ rather than in bytes.
-}
indexByteArray :: (Prim a, Bytes a) => ByteArray -> Int -> a
indexByteArray arr ix = fromBigEndian (PM.indexByteArray arr ix)

{- | Write a primitive value to the byte array. The offset is given
in bytes rather than in elements of type @a@. The most significant
byte in the value comes first.
-}
writeUnalignedByteArray ::
  (PrimMonad m, PrimUnaligned a, Bytes a) =>
  MutableByteArray (PrimState m) ->
  Int ->
  a ->
  m ()
writeUnalignedByteArray arr ix v = PMU.writeUnalignedByteArray arr ix (toBigEndian v)

{- | Read a primitive value from the byte array, interpreting the first
byte as the most significant one. The offset is given in bytes rather
than in elements of type @a@.
-}
readUnalignedByteArray ::
  (PrimMonad m, PrimUnaligned a, Bytes a) =>
  MutableByteArray (PrimState m) ->
  Int ->
  m a
readUnalignedByteArray arr ix = fromBigEndian <$> PMU.readUnalignedByteArray arr ix

{- | Read a primitive value from the byte array, interpreting the first
byte as the most significant one. The offset is given in bytes rather
than in elements of type @a@.
-}
indexUnalignedByteArray ::
  (PrimUnaligned a, Bytes a) =>
  ByteArray ->
  Int ->
  a
indexUnalignedByteArray arr ix = fromBigEndian (PMU.indexUnalignedByteArray arr ix)
