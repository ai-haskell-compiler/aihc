module Sound.MED.Raw.MMD0NoteData where

import Sound.MED.Basic.Amiga

import Data.Bits (shiftR, shiftL, (.&.), (.|.))

data MMD0NoteData = MMD0NoteData
  { note       :: UBYTE
  , instrument :: UBYTE
  , command    :: UBYTE
  , databyte   :: UBYTE
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD0NoteData #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD0NoteData #-}
peek :: (Reader m) => PTR -> m MMD0NoteData
peek p = do
  byte1 <- peekUBYTE (p + 0)
  byte2 <- peekUBYTE (p + 1)
  byte3 <- peekUBYTE (p + 2)
  let x           = (byte1 .&. 0x80) `shiftR` 7
  let y           = (byte1 .&. 0x40) `shiftR` 6
  let nnnnnn      = (byte1 .&. 0x3F)
  let iiii        = (byte2 .&. 0xF0) `shiftR` 4
  let cccc        = (byte2 .&. 0x0F)
  let dddddddd    = (byte3 .&. 0xFF)
  let note'       = nnnnnn
  let instrument' = (y `shiftL` 6) .|. (x `shiftL` 5) .|. iiii
  let command'    = cccc
  let databyte'   = dddddddd
  return $ MMD0NoteData
    note' instrument' command' databyte'
