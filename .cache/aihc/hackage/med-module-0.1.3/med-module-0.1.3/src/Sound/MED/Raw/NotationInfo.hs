module Sound.MED.Raw.NotationInfo where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data NotationInfo = NotationInfo
  { n_of_sharps :: UBYTE
  , flags       :: UBYTE
  , trksel      :: [ WORD ]
  , trkshow     :: [ UBYTE ]
  , trkghost    :: [ UBYTE ]
  , notetr      :: [ BYTE ]
  , pad         :: UBYTE
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader NotationInfo #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader NotationInfo #-}
peek :: (Reader m) => PTR -> m NotationInfo
peek p = do
  n_of_sharps' <- peekUBYTE (p+0)
  flags'       <- peekUBYTE (p+1)
  trksel'      <- mapM peekWORD  $ pointerRange (p+ 2) 2 5
  trkshow'     <- mapM peekUBYTE $ pointerRange (p+22) 1 16
  trkghost'    <- mapM peekUBYTE $ pointerRange (p+38) 1 16
  notetr'      <- mapM peekBYTE  $ pointerRange (p+54) 1 63
  pad'         <- peekUBYTE (p+117)
  return $ NotationInfo
    n_of_sharps' flags' trksel' trkshow' trkghost' notetr' pad'
