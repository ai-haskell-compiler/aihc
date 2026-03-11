{-# LANGUAGE RecordWildCards #-}

-- safe module, only export the safe bits (no @Write(..)@!!)
module Bytezap.Write
  ( Write(writeLength, writeOp)
  , LengthType(ExactLength, MaxLength)
  , runWriteBS, runWriteBSUptoN
  , prim, byteString, byteArray#, replicateByte
  ) where

import Bytezap.Write.Internal

import Bytezap.Poke qualified as Poke
import Bytezap.Poke ( Poke )
import Raehik.Compat.Data.Primitive.Types
import GHC.Exts
import Data.ByteString qualified as BS
import Data.Word ( Word8 )

runWriteBS :: Write ExactLength RealWorld -> BS.ByteString
runWriteBS = runWriteWith Poke.unsafeRunPokeBS

runWriteBSUptoN :: Write MaxLength RealWorld -> BS.ByteString
runWriteBSUptoN = runWriteWith Poke.unsafeRunPokeBSUptoN

-- | Helper for writing 'Write' runners.
runWriteWith :: forall a s lt. (Int -> Poke s -> a) -> Write lt s -> a
runWriteWith runPoke (Write l p) = runPoke l p

prim :: forall a s. Prim' a => a -> Write ExactLength s
prim a = Write (sizeOf (undefined :: a)) (Poke.prim a)

byteString :: BS.ByteString -> Write ExactLength RealWorld
byteString bs = Write (BS.length bs) (Poke.byteString bs)

byteArray# :: ByteArray# -> Int# -> Int# -> Write ExactLength s
byteArray# ba# baos# balen# = Write{..}
  where
    writeLength = I# balen#
    writeOp     = Poke.byteArray# ba# baos# balen#

-- | essentially memset
replicateByte :: Int -> Word8 -> Write ExactLength RealWorld
replicateByte len byte = Write len (Poke.replicateByte len byte)
