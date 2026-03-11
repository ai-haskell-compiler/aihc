module Sound.MED.Raw.PlaySeq where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data PlaySeq = PlaySeq
  { name       :: [ UBYTE ]
  , reserved0  :: ULONG
  , reserved1  :: ULONG
  , len        :: UWORD
  , seq        :: [ UWORD ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader PlaySeq #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader PlaySeq #-}
peek :: (Reader m) => PTR -> m PlaySeq
peek p = do
  name'      <- mapM peekUBYTE $ pointerRange p 1 32
  reserved0' <- peekULONG (p+32)
  reserved1' <- peekULONG (p+36)
  len'       <- peekUWORD (p+40)
  seq'       <- mapM peekUWORD $ pointerRangeGen (p+42) 2 len'
  return $ PlaySeq
    name' reserved0' reserved1' len' seq'
