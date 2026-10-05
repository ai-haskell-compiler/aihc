{-# LANGUAGE MagicHash #-}

-- | The width at which a pointer is stored on wasm32 under WASI.
--
-- A C structure holds a pointer in the width of the platform, which is
-- thirty-two bits here, while an 'Addr#' primitive reads and writes sixty-four.
-- 'Foreign.Storable' reads and writes a pointer as this type, so a field
-- that follows a pointer in a C structure keeps its place.
module Foreign.Storable.Repr
  ( PtrRep,
    ptrToRep,
    ptrFromRep,
  )
where

import GHC.Prim (Addr#, addr2Int#, int2Addr#, int2Word#, word2Int#, word32ToWord#, wordToWord32#)
import GHC.Word (Word32 (..))

type PtrRep = Word32

ptrToRep :: Addr# -> PtrRep
ptrToRep address = W32# (wordToWord32# (int2Word# (addr2Int# address)))

ptrFromRep :: PtrRep -> Addr#
ptrFromRep (W32# word) = int2Addr# (word2Int# (word32ToWord# word))
