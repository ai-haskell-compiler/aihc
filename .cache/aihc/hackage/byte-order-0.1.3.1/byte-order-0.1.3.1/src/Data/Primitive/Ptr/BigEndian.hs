{- | This is drop-in replacement for the read, write, and index functions
present in @Data.Primitive.Ptr@. While the functions from those modules
use native byte order, the functions in this one use big-endian byte order
(most significant byte first).
-}
module Data.Primitive.Ptr.BigEndian
  ( -- * Aligned
    writeOffPtr
  , readOffPtr
  , indexOffPtr
  ) where

import Control.Monad.Primitive (PrimMonad)
import Data.Primitive (Prim)
import Data.Primitive.Ptr (Ptr)
import qualified Data.Primitive.Ptr as PM
import System.ByteOrder (Bytes, fromBigEndian, toBigEndian)

{- | Write a primitive value to the pointer. The offset is given
in elements of type @a@ rather than in bytes. The most significant
byte in the value comes first.
-}
writeOffPtr :: (PrimMonad m, Prim a, Bytes a) => Ptr a -> Int -> a -> m ()
writeOffPtr arr ix v = PM.writeOffPtr arr ix (toBigEndian v)

{- | Read a primitive value from the pointer, interpreting the first
byte as the most significant one. The offset is given in elements of
type @a@ rather than in bytes.
-}
readOffPtr :: (PrimMonad m, Prim a, Bytes a) => Ptr a -> Int -> m a
readOffPtr arr ix = fromBigEndian <$> PM.readOffPtr arr ix

{- | Read a primitive value from the pointer, interpreting the first
byte as the most significant one. The offset is given in elements of
type @a@ rather than in bytes.
-}
indexOffPtr :: (Prim a, Bytes a) => Ptr a -> Int -> a
indexOffPtr arr ix = fromBigEndian (PM.indexOffPtr arr ix)
