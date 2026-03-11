module Sound.MED.Raw.MMD2Song where

import qualified Sound.MED.Raw.MMD0Sample as MMD0Sample
import Sound.MED.Raw.MMD0Sample(MMD0Sample)
import qualified Sound.MED.Raw.PlaySeq as PlaySeq
import Sound.MED.Raw.PlaySeq(PlaySeq)
import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMD2Song = MMD2Song
  { sample        :: [ MMD0Sample ]
  , numblocks     :: UWORD
  , songlen       :: UWORD
  , playseqtable  :: [ PlaySeq ]
  , sectiontable  :: [ UWORD ]
  , trackvols     :: [ UBYTE ]
  , numtracks     :: UWORD
  , numpseqs      :: UWORD
  , trackpans     :: [ BYTE ]
  , flags3        :: ULONG
  , voladj        :: UWORD
  , channels      :: UWORD
  , mix_echotype  :: UBYTE
  , mix_echodepth :: UBYTE
  , mix_echolen   :: UWORD
  , mix_stereosep :: BYTE
  , pad0          :: [ UBYTE ]
  , deftempo      :: UWORD
  , playtransp    :: BYTE
  , flags         :: UBYTE
  , flags2        :: UBYTE
  , tempo2        :: UBYTE
  , pad1          :: [ UBYTE ]
  , mastervol     :: UBYTE
  , numsamples    :: UBYTE
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD2Song #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD2Song #-}
peek :: (Reader m) => PTR -> m MMD2Song
peek p = do
  sample'         <- mapM MMD0Sample.peek $ pointerRange p 8 63
  numblocks'      <- peekUWORD (p+504)
  songlen'        <- peekUWORD (p+506)
  numpseqs'       <- peekUWORD (p+522)
  playseqtable''' <- peekPTR   (p+508)
  playseqtable''  <- mapM peekPTR $ pointerRangeGen playseqtable''' 4 numpseqs'
  playseqtable'   <- mapM PlaySeq.peek playseqtable''
  sectiontable''  <- peekPTR   (p+512)
  sectiontable'   <- mapM peekUWORD $ pointerRangeGen sectiontable'' 2 songlen'
  numtracks'      <- peekUWORD (p+520)
  trackvols''     <- peekPTR   (p+516)
  trackvols'      <- mapM peekUBYTE $ pointerRangeGen trackvols'' 1 numtracks'
  trackpans''     <- peekPTR   (p+524)
  trackpans'      <- if trackpans'' == 0 then return $ replicate (fromIntegral numtracks') 0 else mapM peekBYTE $ pointerRangeGen trackpans'' 1 numtracks'
  flags3'         <- peekULONG (p+528)
  voladj'         <- peekUWORD (p+532)
  channels'       <- peekUWORD (p+534)
  mix_echotype'   <- peekUBYTE (p+536)
  mix_echodepth'  <- peekUBYTE (p+537)
  mix_echolen'    <- peekUWORD (p+538)
  mix_stereosep'  <- peekBYTE  (p+540)
  pad0'           <- mapM peekUBYTE $ pointerRange (p+541) 1 223
  deftempo'       <- peekUWORD (p+764)
  playtransp'     <- peekBYTE  (p+766)
  flags'          <- peekUBYTE (p+767)
  flags2'         <- peekUBYTE (p+768)
  tempo2'         <- peekUBYTE (p+769)
  pad1'           <- mapM peekUBYTE $ pointerRange (p+770) 1 16
  mastervol'      <- peekUBYTE (p+786)
  numsamples'     <- peekUBYTE (p+787)
  return $ MMD2Song
    sample' numblocks' songlen' playseqtable' sectiontable' trackvols'
    numtracks' numpseqs' trackpans' flags3' voladj' channels'
    mix_echotype' mix_echodepth' mix_echolen' mix_stereosep' pad0'
    deftempo' playtransp' flags' flags2' tempo2' pad1' mastervol'
    numsamples'
