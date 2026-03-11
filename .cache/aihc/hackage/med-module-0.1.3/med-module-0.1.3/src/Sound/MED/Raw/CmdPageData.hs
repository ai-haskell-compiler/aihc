module Sound.MED.Raw.CmdPageData where

import Sound.MED.Basic.Amiga

data CmdPageData = CmdPageData
  { command    :: UBYTE
  , databyte   :: UBYTE
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader CmdPageData #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader CmdPageData #-}
peek :: (Reader m) => PTR -> m CmdPageData
peek p = do
  command'  <- peekUBYTE (p + 0)
  databyte' <- peekUBYTE (p + 1)
  return $ CmdPageData
    command' databyte'
