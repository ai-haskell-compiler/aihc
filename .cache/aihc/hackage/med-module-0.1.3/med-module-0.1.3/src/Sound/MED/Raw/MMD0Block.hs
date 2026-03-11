module Sound.MED.Raw.MMD0Block where

import qualified Sound.MED.Raw.MMD0NoteData as MMD0NoteData
import Sound.MED.Raw.MMD0NoteData(MMD0NoteData)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMD0Block = MMD0Block
  { numtracks :: UBYTE
  , lines     :: UBYTE -- NOTE: actual number of lines is one greater
  , notedata  :: [ [ MMD0NoteData ] ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD0Block #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD0Block #-}
peek :: (Reader m) => PTR -> m MMD0Block
peek p = do
  numtracks'    <- peekUBYTE (p + 0)
  lines'        <- peekUBYTE (p + 1)
  notedata''    <- mapM MMD0NoteData.peek $ pointerRangeGen2 (p+2) 3 numtracks' (lines'+1)
  let notedata' =  chunk numtracks' notedata''
  return $ MMD0Block
    numtracks' lines' notedata'
