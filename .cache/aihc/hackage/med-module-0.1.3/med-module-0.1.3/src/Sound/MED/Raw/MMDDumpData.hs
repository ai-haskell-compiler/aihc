module Sound.MED.Raw.MMDDumpData where

import qualified Sound.MED.Raw.MMDDump as MMDDump
import Sound.MED.Raw.MMDDump(MMDDump)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMDDumpData = MMDDumpData
  { numdumps :: UWORD
  , reserved :: [ UWORD ]
  , dumps    :: [ MMDDump ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMDDumpData #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMDDumpData #-}
peek :: (Reader m) => PTR -> m MMDDumpData
peek p = do
  numdumps' <- peekUWORD p
  reserved' <- mapM peekUWORD $ pointerRange (p+2) 2 3
  dumps''   <- mapM peekPTR $ pointerRangeGen (p+8) 4 numdumps'
  dumps'    <- mapM MMDDump.peek dumps''
  return $ MMDDumpData
    numdumps' reserved' dumps'
