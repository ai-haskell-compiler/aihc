module Sound.MED.Raw.MMD0Song where

import qualified Sound.MED.Raw.MMD0Sample as MMD0Sample
import Sound.MED.Raw.MMD0Sample(MMD0Sample)
import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMD0Song = MMD0Song
  { sample     :: [ MMD0Sample ]
  , numblocks  :: UWORD
  , songlen    :: UWORD
  , playseq    :: [ UBYTE ]
  , deftempo   :: UWORD
  , playtransp :: BYTE
  , flags      :: UBYTE
  , flags2     :: UBYTE
  , tempo2     :: UBYTE
  , trkvol     :: [ UBYTE ]
  , mastervol  :: UBYTE
  , numsamples :: UBYTE
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD0Song #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD0Song #-}
peek :: (Reader m) => PTR -> m MMD0Song
peek p = do
  sample'      <- mapM MMD0Sample.peek $ pointerRange p 8 63
  numblocks'   <- peekUWORD (p+504)
  songlen'     <- peekUWORD (p+506)
  playseq'     <- mapM peekUBYTE $ pointerRange (p+508) 1 256
  deftempo'    <- peekUWORD (p+764)
  playtransp'  <- peekBYTE  (p+766)
  flags'       <- peekUBYTE (p+767)
  flags2'      <- peekUBYTE (p+768)
  tempo2'      <- peekUBYTE (p+769)
  trkvol'      <- mapM peekUBYTE $ pointerRange (p+770) 1 16
  mastervol'   <- peekUBYTE (p+786)
  numsamples'  <- peekUBYTE (p+787)
  return $ MMD0Song
    sample' numblocks' songlen' playseq' deftempo' playtransp' flags'
    flags2' tempo2' trkvol' mastervol' numsamples'
