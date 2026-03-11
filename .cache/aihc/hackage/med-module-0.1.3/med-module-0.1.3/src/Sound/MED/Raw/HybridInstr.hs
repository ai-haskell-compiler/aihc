module Sound.MED.Raw.HybridInstr where

import qualified Sound.MED.Raw.SynthWF as SynthWF
import Sound.MED.Raw.SynthWF(SynthWF)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

import Control.Monad (liftM)


data HybridInstr = HybridInstr
  { defaultdecay :: UBYTE
  , reserved     :: [ UBYTE ]
  , rep          :: UWORD
  , replen       :: UWORD
  , voltbllen    :: UWORD
  , wftbllen     :: UWORD
  , volspeed     :: UBYTE
  , wfspeed      :: UBYTE
  , wforms       :: UWORD
  , voltbl       :: [ UBYTE ]
  , wftbl        :: [ UBYTE ]
  , wf0          :: PTR -- FIXME
  , wf           :: [ Maybe SynthWF ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader HybridInstr #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader HybridInstr #-}
peek :: (Reader m) => PTR -> m HybridInstr
peek p = do
  let nonEmpty (x:xs) = (x,xs)
      nonEmpty [] = error "list must be non-empty"
  defaultdecay' <- peekUBYTE (p+6)
  reserved'     <- mapM peekUBYTE $ pointerRange (p+7) 1 3
  rep'          <- peekUWORD (p+10)
  replen'       <- peekUWORD (p+12)
  voltbllen'    <- peekUWORD (p+14)
  wftbllen'     <- peekUWORD (p+16)
  volspeed'     <- peekUBYTE (p+18)
  wfspeed'      <- peekUBYTE (p+19)
  wforms'       <- peekUWORD (p+20)
  voltbl'       <- mapM peekUBYTE $ pointerRange (p+22) 1 128
  wftbl'        <- mapM peekUBYTE $ pointerRange (p+150) 150 128
  (wf0',wf'')   <- liftM nonEmpty $
                   mapM peekPTR   $ pointerRange (p+278) 4 64
  wf'           <- mapM (SynthWF.peek . (p+) $?) wf''
  return $ HybridInstr
    defaultdecay' reserved' rep' replen' voltbllen' wftbllen'
    volspeed' wfspeed' wforms' voltbl' wftbl' wf0' wf'
