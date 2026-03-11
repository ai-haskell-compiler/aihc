module Sound.MED.Raw.SynthInstr where

import qualified Sound.MED.Raw.SynthWF as SynthWF
import Sound.MED.Raw.SynthWF(SynthWF)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data SynthInstr = SynthInstr
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
  , wf           :: [ Maybe SynthWF ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader SynthInstr #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader SynthInstr #-}
peek :: (Reader m) => PTR -> m SynthInstr
peek p = do
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
  wf''          <- mapM peekPTR   $ pointerRange (p+278) 4 64
  wf'           <- mapM (SynthWF.peek . (p+) $?) wf''
  return $ SynthInstr
    defaultdecay' reserved' rep' replen' voltbllen' wftbllen'
    volspeed' wfspeed' wforms' voltbl' wftbl' wf'

{-
showVOL :: [UBYTE] -> String
showVOL vs = "[ " ++ (unwords . map showVOL') vs ++ " ]"
showVOL' :: UBYTE -> String
showVOL' 0xF0 = "SPD"
showVOL' 0xF1 = "WAI"
showVOL' 0xF2 = "CHD"
showVOL' 0xF3 = "CDU"
showVOL' 0xF4 = "EN1"
showVOL' 0xF5 = "EN2"
showVOL' 0xF6 = "EST"
showVOL' 0xFA = "JWS"
showVOL' 0xFB = "HLT"
showVOL' 0xFE = "JMP"
showVOL' 0xFF = "END"
showVOL' v    = show v

showWF :: [UBYTE] -> String
showWF vs = "[ " ++ (unwords . map showWF') vs ++ " ]"
showWF' :: UBYTE -> String
showWF' 0xF0 = "SPD"
showWF' 0xF1 = "WAI"
showWF' 0xF2 = "CHD"
showWF' 0xF3 = "CDU"
showWF' 0xF4 = "VBD"
showWF' 0xF5 = "VBS"
showWF' 0xF6 = "RES"
showWF' 0xF7 = "VWF"
showWF' 0xFA = "JVS"
showWF' 0xFB = "HLT"
showWF' 0xFC = "ARP"
showWF' 0xFD = "ARE"
showWF' 0xFE = "JMP"
showWF' 0xFF = "END"
showWF' v    = show v
-}
