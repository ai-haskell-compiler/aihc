module Sound.MED.Raw.InstrHdr where

import qualified Sound.MED.Raw.HybridInstr as HybridInstr
import Sound.MED.Raw.HybridInstr(HybridInstr)
import qualified Sound.MED.Raw.SynthInstr as SynthInstr
import Sound.MED.Raw.SynthInstr(SynthInstr)
import qualified Sound.MED.Raw.SampleInstr as SampleInstr
import Sound.MED.Raw.SampleInstr(SampleInstr)

import Sound.MED.Basic.Amiga

import Data.Bits ((.&.))

data Instrument =
    Unknown
  | Hybrid HybridInstr
  | Synthetic SynthInstr
  | Sample SampleInstr
  deriving (Show)

data InstrHdr = InstrHdr
  { len    :: ULONG
  , stype  :: WORD
  , s16    :: Bool
  , stereo :: Bool
  , md16   :: Bool
  , dat    :: Instrument
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader InstrHdr #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader InstrHdr #-}
peek :: (Reader m) => PTR -> m InstrHdr
peek p = do
  len'   <- peekULONG  p
  itype' <- peekWORD  (p+4)
  let s16'    = (itype' .&. 0x10) == 0x10
  let stereo' = (itype' .&. 0x20) == 0x20
  let md16'   = itype'            == 0x18
  let stype'  = if itype' < 0 then itype' else itype' .&. 0x7
  dat' <- case stype' of
    (-2) -> do
      hybridinstr' <- HybridInstr.peek p
      return $ Hybrid hybridinstr'
    (-1) -> do
      synthinstr' <- SynthInstr.peek p
      return $ Synthetic synthinstr'
    _ -> if stype' < 0 || 7 < stype' then return Unknown else do
      sampleinstr' <- SampleInstr.peek len' stype' s16' stereo' p
      return $ Sample sampleinstr'
  return $ InstrHdr
    len' stype' s16' stereo' md16' dat'
