module Sound.MED.Raw.MMDInstrInfo where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMDInstrInfo = MMDInstrInfo
  { name :: Maybe [ UBYTE ]
  }
  deriving (Show)

{-# SPECIALISE peek :: UWORD -> PTR -> StorableReader MMDInstrInfo #-}
{-# SPECIALISE peek :: UWORD -> PTR -> ByteStringReader MMDInstrInfo #-}
peek :: (Reader m) => UWORD -> PTR -> m MMDInstrInfo
peek size p = do
  name' <- skipIf (size < 40) $ mapM peekUBYTE $ pointerRange p 1 40
  return $ MMDInstrInfo
    name'
