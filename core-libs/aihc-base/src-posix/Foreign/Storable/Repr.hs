{-# LANGUAGE MagicHash #-}

-- | The width at which a pointer is stored on a 64-bit POSIX platform.
--
-- A C structure holds a pointer in the width of the platform, which is
-- sixty-four bits here, the width of an 'Addr#' primitive.
-- 'Foreign.Storable' reads and writes a pointer as this type.
module Foreign.Storable.Repr
  ( PtrRep,
    ptrToRep,
    ptrFromRep,
  )
where

import GHC.Prim (Addr#, addr2Int#, int2Addr#, int2Word#, word2Int#, word64ToWord#, wordToWord64#)
import GHC.Word (Word64 (..))

type PtrRep = Word64

ptrToRep :: Addr# -> PtrRep
ptrToRep address = W64# (wordToWord64# (int2Word# (addr2Int# address)))

ptrFromRep :: PtrRep -> Addr#
ptrFromRep (W64# word) = int2Addr# (word2Int# (word64ToWord# word))
