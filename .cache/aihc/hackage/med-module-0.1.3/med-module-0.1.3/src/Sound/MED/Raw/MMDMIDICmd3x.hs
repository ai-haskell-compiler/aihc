module Sound.MED.Raw.MMDMIDICmd3x where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMDMIDICmd3x = MMDMIDICmd3x
  { struct_ver      :: UBYTE
  , pad             :: UBYTE
  , num_of_settings :: UWORD
  , ctrlr_types     :: [ UBYTE ]
  , ctrlr_numbers   :: [ UWORD ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMDMIDICmd3x #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMDMIDICmd3x #-}
peek :: (Reader m) => PTR -> m MMDMIDICmd3x
peek p = do
  struct_ver'      <- peekUBYTE (p+0)
  pad'             <- peekUBYTE (p+1)
  num_of_settings' <- peekUWORD (p+2)
  ctrlr_types''    <- peekPTR   (p+4)
  ctrlr_types'     <- mapM peekUBYTE $ pointerRangeGen ctrlr_types'' 1 num_of_settings'
  ctrlr_numbers''  <- peekPTR   (p+8)
  ctrlr_numbers'   <- mapM peekUWORD $ pointerRangeGen ctrlr_numbers'' 2 num_of_settings'
  return $ MMDMIDICmd3x
    struct_ver' pad' num_of_settings' ctrlr_types' ctrlr_numbers'
