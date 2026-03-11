module Sound.MED.Raw.MMD0Sample where

import Sound.MED.Basic.Amiga

data MMD0Sample = MMD0Sample
  { rep        :: UWORD
  , replen     :: UWORD
  , midich     :: UBYTE
  , midipreset :: UBYTE
  , svol       :: UBYTE
  , strans     :: BYTE
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD0Sample #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD0Sample #-}
peek :: (Reader m) => PTR -> m MMD0Sample
peek p = do
  rep'        <- peekUWORD (p+0)
  replen'     <- peekUWORD (p+2)
  midich'     <- peekUBYTE (p+4)
  midipreset' <- peekUBYTE (p+5)
  svol'       <- peekUBYTE (p+6)
  strans'     <- peekBYTE  (p+7)
  return $ MMD0Sample
    rep' replen' midich' midipreset' svol' strans'
