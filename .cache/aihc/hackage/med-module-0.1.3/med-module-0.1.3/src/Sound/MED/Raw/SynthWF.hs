module Sound.MED.Raw.SynthWF where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data SynthWF = SynthWF
  { len2   :: UWORD
  , wfdata :: [ BYTE ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader SynthWF #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader SynthWF #-}
peek :: (Reader m) => PTR -> m SynthWF
peek p = do
  len2'   <- peekUWORD (p+0)
  wfdata' <- mapM peekBYTE $ pointerRangeGen (p+2) 1 (len2'*2)
  return $ SynthWF
    len2' wfdata'
