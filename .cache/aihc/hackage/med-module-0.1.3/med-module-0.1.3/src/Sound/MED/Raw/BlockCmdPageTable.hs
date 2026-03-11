module Sound.MED.Raw.BlockCmdPageTable where

import qualified Sound.MED.Raw.CmdPageData as CmdPageData
import Sound.MED.Raw.CmdPageData(CmdPageData)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

import Control.Monad (liftM)

data BlockCmdPageTable = BlockCmdPageTable
  { num_pages :: UWORD
  , reserved  :: UWORD
  , pages     :: [ Maybe [ [ CmdPageData ] ] ]
  }
  deriving (Show)

{-# SPECIALISE peek :: UWORD -> UWORD -> PTR -> StorableReader BlockCmdPageTable #-}
{-# SPECIALISE peek :: UWORD -> UWORD -> PTR -> ByteStringReader BlockCmdPageTable #-}
peek :: (Reader m) => UWORD -> UWORD -> PTR -> m BlockCmdPageTable
peek tracks numlines p = do
  num_pages' <- peekUWORD p
  reserved'  <- peekUWORD (p+2)
  pages''    <- mapM peekPTR $ pointerRangeGen (p+4) 4 num_pages'
  pages'     <- mapM (peekPage tracks numlines) pages''
  return $ BlockCmdPageTable
    num_pages' reserved' pages'

peekPage ::
  (Reader m) => UWORD -> UWORD -> PTR -> m (Maybe [ [ CmdPageData ] ])
peekPage tracks numlines p =
  skipIf (p == 0) $ liftM (chunk tracks) $
    mapM CmdPageData.peek $ pointerRangeGen2 p 2 tracks numlines
