module Sound.MED.Raw.MMD1NoteData where

import Sound.MED.Basic.Amiga

import Data.Bits ((.&.))

data MMD1NoteData = MMD1NoteData
  { note       :: UBYTE
  , instrument :: UBYTE
  , command    :: UBYTE
  , databyte   :: UBYTE
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD1NoteData #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD1NoteData #-}
peek :: (Reader m) => PTR -> m MMD1NoteData
peek p = do
  byte1 <- peekUBYTE (p + 0)
  byte2 <- peekUBYTE (p + 1)
  byte3 <- peekUBYTE (p + 2)
  byte4 <- peekUBYTE (p + 3)
  let nnnnnnn     = byte1 .&. 0x7F
  let iiiiii      = byte2 .&. 0x3F
  let cccccccc    = byte3 .&. 0xFF
  let dddddddd    = byte4 .&. 0xFF
  let note'       = nnnnnnn
  let instrument' = iiiiii
  let command'    = cccccccc
  let databyte'   = dddddddd
  return $ MMD1NoteData
    note' instrument' command' databyte'
