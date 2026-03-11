module Sound.MED.Raw.MMDDump where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMDDump = MMDDump
  { len     :: ULONG
  , dat     :: [ UBYTE ]
  , ext_len :: UWORD
  , name    :: Maybe [ UBYTE ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMDDump #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMDDump #-}
peek :: (Reader m) => PTR -> m MMDDump
peek p = do
  len'     <- peekULONG (p+0)
  dat''    <- peekPTR   (p+4)
  dat'     <- mapM peekUBYTE $ pointerRangeGenCheck dat'' 1 len'
  ext_len' <- peekUWORD (p+8)
  name'    <-
    skipIf (ext_len' < 20) $ mapM peekUBYTE $ pointerRange (p+10) 1 20
  return $ MMDDump
    len' dat' ext_len' name'
