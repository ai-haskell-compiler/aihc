module Sound.MED.Raw.MMDARexxTrigCmd where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMDARexxTrigCmd = MMDARexxTrigCmd
  { next     :: Maybe MMDARexxTrigCmd
  , cmdnum   :: UBYTE
  , pad      :: UBYTE
  , cmdtype  :: WORD
  , cmd      :: [ UBYTE ]
  , port     :: [ UBYTE ]
  , cmd_len  :: UWORD
  , port_len :: UWORD
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMDARexxTrigCmd #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMDARexxTrigCmd #-}
peek :: (Reader m) => PTR -> m MMDARexxTrigCmd
peek p = do
  next'''    <- peekPTR p
  next'      <- peek $? next'''
  cmdnum'    <- peekUBYTE (p+4)
  pad'       <- peekUBYTE (p+5)
  cmdtype'   <- peekWORD  (p+6)
  cmd''      <- peekPTR   (p+8)
  port''     <- peekPTR   (p+12)
  cmd_len'   <- peekUWORD (p+16)
  port_len'  <- peekUWORD (p+18)
  cmd'       <- mapM peekUBYTE $ pointerRangeGenCheck cmd'' 1 (cmd_len'+1)
  port'      <- mapM peekUBYTE $ pointerRangeGenCheck port'' 1 (port_len'+1)
  return $ MMDARexxTrigCmd
    next' cmdnum' pad' cmdtype' cmd' port' cmd_len' port_len'
