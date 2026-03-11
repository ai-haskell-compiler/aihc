module Sound.MED.Raw.MMD1Block where

import qualified Sound.MED.Raw.BlockInfo as BlockInfo
import Sound.MED.Raw.BlockInfo(BlockInfo)
import qualified Sound.MED.Raw.MMD1NoteData as MMD1NoteData
import Sound.MED.Raw.MMD1NoteData(MMD1NoteData)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data MMD1Block = MMD1Block
  { numtracks :: UWORD
  , lines     :: UWORD -- NOTE: actual number of lines is one greater
  , info      :: Maybe BlockInfo
  , notedata  :: [ [ MMD1NoteData ] ]
  }
  deriving (Show)

{-# SPECIALISE peek :: PTR -> StorableReader MMD1Block #-}
{-# SPECIALISE peek :: PTR -> ByteStringReader MMD1Block #-}
peek :: (Reader m) => PTR -> m MMD1Block
peek p = do
  numtracks'    <- peekUWORD (p + 0)
  lines'        <- peekUWORD (p + 2)
  info'''       <- peekPTR   (p + 4)
  info'         <- BlockInfo.peek numtracks' (lines'+1) $? info'''
  notedata''    <- mapM MMD1NoteData.peek $ pointerRangeGen2 (p+8) 4 numtracks' (lines'+1)
  let notedata' =  chunk numtracks' notedata''
  return $ MMD1Block
    numtracks' lines' info' notedata'
