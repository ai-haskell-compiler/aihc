module Sound.MED.Raw.MMD3 where

import qualified Sound.MED.Raw.MMD2Song as MMD2Song
import Sound.MED.Raw.MMD2Song(MMD2Song)
import qualified Sound.MED.Raw.MMD1Block as MMD1Block
import Sound.MED.Raw.MMD1Block(MMD1Block)
import qualified Sound.MED.Raw.InstrHdr as InstrHdr
import Sound.MED.Raw.InstrHdr(InstrHdr)
import qualified Sound.MED.Raw.MMD0exp as MMD0exp
import Sound.MED.Raw.MMD0exp(MMD0exp)
import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMD3 = MMD3
  { id          :: ULONG
  , modlen      :: ULONG
  , song        :: MMD2Song
  , psecnum     :: UWORD
  , pseq        :: UWORD
  , blockarr    :: [ MMD1Block ]
  , mmdflags    :: UBYTE
  , reserved1   :: UBYTE
  , reserved2   :: UBYTE
  , reserved3   :: UBYTE
  , smplarr     :: [ Maybe InstrHdr ]
  , reserved4   :: ULONG
  , expdata     :: Maybe MMD0exp
  , reserved5   :: ULONG
  , pstate      :: UWORD
  , pblock      :: UWORD
  , pline       :: UWORD
  , pseqnum     :: UWORD
  , actplayline :: WORD
  , counter     :: UBYTE
  , extra_songs :: UBYTE
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD3 #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD3 #-}
peek :: (Reader m) => PTR -> m MMD3
peek p = do
  id'          <- peekULONG (p + 0)
  modlen'      <- peekULONG (p + 4)
  song''       <- peekPTR   (p + 8)
  song'        <- MMD2Song.peek song''
  psecnum'     <- peekUWORD (p + 12)
  pseq'        <- peekUWORD (p + 14)
  blockarr'''  <- peekPTR   (p + 16)
  blockarr''   <- mapM peekPTR $ pointerRangeGen blockarr''' 4 (MMD2Song.numblocks song')
  blockarr'    <- mapM MMD1Block.peek blockarr''
  mmdflags'    <- peekUBYTE (p + 20)
  reserved1'   <- peekUBYTE (p + 21)
  reserved2'   <- peekUBYTE (p + 22)
  reserved3'   <- peekUBYTE (p + 23)
  smplarr'''   <- peekPTR   (p + 24)
  smplarr''    <- mapM peekPTR $ pointerRangeGenCheck smplarr''' 4 (MMD2Song.numsamples song')
  smplarr'     <- mapM (InstrHdr.peek $?) smplarr''
  reserved4'   <- peekULONG (p + 28)
  expdata'''   <- peekPTR   (p + 32)
  expdata'     <- MMD0exp.peek $? expdata'''
  reserved5'   <- peekULONG (p + 36)
  pstate'      <- peekUWORD (p + 40)
  pblock'      <- peekUWORD (p + 42)
  pline'       <- peekUWORD (p + 44)
  pseqnum'     <- peekUWORD (p + 46)
  actplayline' <- peekWORD  (p + 48)
  counter'     <- peekUBYTE (p + 50)
  extra_songs' <- peekUBYTE (p + 51)
  return $ MMD3
    id' modlen' song' psecnum' pseq' blockarr' mmdflags'
    reserved1' reserved2' reserved3' smplarr' reserved4'
    expdata' reserved5' pstate' pblock' pline' pseqnum'
    actplayline' counter' extra_songs'
