module Sound.MED.Raw.MMDInfo where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMDInfo = MMDInfo
  { next     :: Maybe MMDInfo
  , reserved :: UWORD
  , typ      :: UWORD
  , len      :: ULONG
  , dat      :: [ UBYTE ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMDInfo #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMDInfo #-}
peek :: (Reader m) => PTR -> m MMDInfo
peek p = do
  next'''   <- peekPTR p
  next'     <- peek $? next'''
  reserved' <- peekUWORD (p+4)
  typ'      <- peekUWORD (p+6)
  len'      <- peekULONG (p+8)
  dat'      <- mapM peekUBYTE $ pointerRangeGen (p+12) 1 len'
  return $ MMDInfo
    next' reserved' typ' len' dat'
