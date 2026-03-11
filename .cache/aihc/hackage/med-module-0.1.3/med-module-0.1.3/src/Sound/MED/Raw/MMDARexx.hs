module Sound.MED.Raw.MMDARexx where

import qualified Sound.MED.Raw.MMDARexxTrigCmd as MMDARexxTrigCmd
import Sound.MED.Raw.MMDARexxTrigCmd(MMDARexxTrigCmd)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMDARexx = MMDARexx
  { reserved   :: UWORD
  , trigcmdlen :: UWORD
  , trigcmd    :: Maybe MMDARexxTrigCmd
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMDARexx #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMDARexx #-}
peek :: (Reader m) => PTR -> m MMDARexx
peek p = do
  reserved'   <- peekUWORD (p+0)
  trigcmdlen' <- peekUWORD (p+2)
  trigcmd'''  <- peekPTR   (p+4)
  trigcmd'    <- MMDARexxTrigCmd.peek $? trigcmd'''
  return $ MMDARexx
    reserved' trigcmdlen' trigcmd'
