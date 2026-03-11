module Sound.MED.Raw.BlockInfo where

import qualified Sound.MED.Raw.BlockCmdPageTable as BlockCmdPageTable
import Sound.MED.Raw.BlockCmdPageTable(BlockCmdPageTable)

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data BlockInfo = BlockInfo
  { hlmask       :: Maybe [ ULONG ]
  , blockname    :: Maybe [ UBYTE ]
  , blocknamelen :: ULONG
  , pagetable    :: Maybe BlockCmdPageTable
  , reserved     :: [ ULONG ]
  }
  deriving (Show)

{-# SPECIALISE peek :: UWORD -> UWORD -> PTR -> StorableReader BlockInfo #-}
{-# SPECIALISE peek :: UWORD -> UWORD -> PTR -> ByteStringReader BlockInfo #-}
peek :: (Reader m) => UWORD -> UWORD -> PTR -> m BlockInfo
peek tracks numlines p = do
  let hlmasklen = (numlines + 31) `div` 32
  hlmask'''     <- peekPTR p
  hlmask'       <-
    skipIf (hlmask''' == 0) $
    mapM peekULONG $ pointerRangeGen hlmask''' 4 hlmasklen
  blocknamelen' <- peekULONG (p+8)
  blockname'''  <- peekPTR (p+4)
  blockname'    <-
    skipIf (blockname''' == 0) $
    mapM peekUBYTE $ pointerRangeGen blockname''' 1 blocknamelen'
  pagetable'''  <- peekPTR (p+12)
  pagetable'    <- BlockCmdPageTable.peek tracks numlines $? pagetable'''
  reserved'     <- mapM peekULONG $ pointerRange (p+16) 4 5
  return $ BlockInfo
    hlmask' blockname' blocknamelen' pagetable' reserved'
