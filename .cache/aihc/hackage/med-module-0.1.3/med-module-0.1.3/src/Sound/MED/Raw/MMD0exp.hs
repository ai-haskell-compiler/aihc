module Sound.MED.Raw.MMD0exp where

import qualified Sound.MED.Raw.InstrExt as InstrExt
import Sound.MED.Raw.InstrExt(InstrExt)
import qualified Sound.MED.Raw.MMDInstrInfo as MMDInstrInfo
import Sound.MED.Raw.MMDInstrInfo(MMDInstrInfo)
import qualified Sound.MED.Raw.NotationInfo as NotationInfo
import Sound.MED.Raw.NotationInfo(NotationInfo)
import qualified Sound.MED.Raw.MMDDumpData as MMDDumpData
import Sound.MED.Raw.MMDDumpData(MMDDumpData)
import qualified Sound.MED.Raw.MMDInfo as MMDInfo
import Sound.MED.Raw.MMDInfo(MMDInfo)
import qualified Sound.MED.Raw.MMDARexx as MMDARexx
import Sound.MED.Raw.MMDARexx(MMDARexx)
import qualified Sound.MED.Raw.MMDMIDICmd3x as MMDMIDICmd3x
import Sound.MED.Raw.MMDMIDICmd3x(MMDMIDICmd3x)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMD0exp = MMD0exp
  { nextmod       :: PTR -- FIXME
  , exp_smp       :: [ InstrExt ]
  , s_ext_entries :: UWORD
  , s_ext_entrsz  :: UWORD
  , annotxt       :: [ UBYTE ]
  , annolen       :: ULONG
  , iinfo         :: [ MMDInstrInfo ]
  , i_ext_entries :: UWORD
  , i_ext_entrsz  :: UWORD
  , jumpmask      :: ULONG
  , rgbtable      :: [ UWORD ]
  , channelsplit  :: [ UBYTE ]
  , n_info        :: Maybe NotationInfo
  , songname      :: [ UBYTE ]
  , songnamelen   :: ULONG
  , dumps         :: Maybe MMDDumpData
  , mmdinfo       :: Maybe MMDInfo
  , mmdrexx       :: Maybe MMDARexx
  , mmdcmd3x      :: Maybe MMDMIDICmd3x
  , reserved2     :: [ ULONG ]
  , tag_end       :: ULONG
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD0exp #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD0exp #-}
peek :: (Reader m) => PTR -> m MMD0exp
peek p = do
  nextmod'       <- peekPTR   (p+0)
  exp_smp''      <- peekPTR   (p+4)
  s_ext_entries' <- peekUWORD (p+8)
  s_ext_entrsz'  <- peekUWORD (p+10)
  exp_smp'       <- mapM (InstrExt.peek s_ext_entrsz') $ pointerRangeGenCheck exp_smp'' (fromIntegral s_ext_entrsz') s_ext_entries'
  annotxt''      <- peekPTR   (p+12)
  annolen'       <- peekULONG (p+16)
  annotxt'       <- mapM peekUBYTE $ pointerRangeGenCheck annotxt'' 1 annolen'
  iinfo''        <- peekPTR   (p+20)
  i_ext_entries' <- peekUWORD (p+24)
  i_ext_entrsz'  <- peekUWORD (p+26)
  iinfo'         <- mapM (MMDInstrInfo.peek i_ext_entrsz') $ pointerRangeGenCheck iinfo'' (fromIntegral i_ext_entrsz') i_ext_entries'
  jumpmask'      <- peekULONG (p+28)
  rgbtable''     <- peekPTR   (p+32)
  rgbtable'      <- mapM peekUWORD $ pointerRangeGenCheck rgbtable'' 2 (8::Int)
  channelsplit'  <- mapM peekUBYTE $ pointerRange (p+36) 1 4
  n_info'''      <- peekPTR   (p+40)
  n_info'        <- NotationInfo.peek $? n_info'''
  songname''     <- peekPTR   (p+44)
  songnamelen'   <- peekULONG (p+48)
  songname'      <- mapM peekUBYTE $ pointerRangeGenCheck songname'' 1 songnamelen'
  dumps'''       <- peekPTR   (p+52)
  dumps'         <- MMDDumpData.peek $? dumps'''
  mmdinfo'''     <- peekPTR   (p+56)
  mmdinfo'       <- MMDInfo.peek $? mmdinfo'''
  mmdrexx'''     <- peekPTR   (p+60)
  mmdrexx'       <- MMDARexx.peek $? mmdrexx'''
  mmdcmd3x'''    <- peekPTR   (p+64)
  mmdcmd3x'      <- MMDMIDICmd3x.peek $? mmdcmd3x'''
  reserved2'     <- mapM peekULONG $ pointerRange (p+68) 4 3
  tag_end'       <- peekULONG (p+80)
  return $ MMD0exp
    nextmod' exp_smp' i_ext_entries' s_ext_entrsz' annotxt' annolen'
    iinfo' i_ext_entries' i_ext_entrsz' jumpmask' rgbtable'
    channelsplit' n_info' songname' songnamelen' dumps' mmdinfo'
    mmdrexx' mmdcmd3x' reserved2' tag_end'
