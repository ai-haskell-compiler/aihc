{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module PureSAT.PartialAssignment where

#define ASSERTING(x)

import PureSAT.Base
import PureSAT.LBool
import PureSAT.LitVar
import PureSAT.Prim

-------------------------------------------------------------------------------
-- Partial Assignment
-------------------------------------------------------------------------------

newtype PartialAssignment s = PA (MutableByteArray s)

newPartialAssignment :: Int -> ST s (PartialAssignment s)
newPartialAssignment (max 4096 -> size) = do
    arr <- newByteArray size
    shrinkMutableByteArray arr size
    fillByteArray arr 0 size 0xff
    return (PA arr)

clonePartialAssignment :: PartialAssignment s -> ST s (PartialAssignment s)
clonePartialAssignment (PA old) = do
    n <- getSizeofMutableByteArray old
    new <- newByteArray n
    copyMutableByteArray new 0 old 0 n
    return (PA new)    

copyPartialAssignment :: PartialAssignment s -> PartialAssignment s -> ST s ()
copyPartialAssignment (PA src) (PA tgt) = do
    n <- getSizeofMutableByteArray src
    m <- getSizeofMutableByteArray tgt
    let size = min n m
    copyMutableByteArray tgt 0 src 0 size

extendPartialAssignment :: PartialAssignment s -> ST s (PartialAssignment s)
extendPartialAssignment (PA arr) = do
    size <- getSizeofMutableByteArray arr
    arr' <- resizeMutableByteArray arr (size + 1)
    writeByteArray arr' size (0xff :: Word8)
    return (PA arr')

lookupPartialAssignment :: Lit -> PartialAssignment s -> ST s LBool
lookupPartialAssignment (MkLit l) (PA arr) = do
    readByteArray arr (lit_to_var l) >>= \case
        0x0 -> return (if y then LFalse else LTrue)
        0x1 -> return (if y then LTrue else LFalse)
        _   -> return LUndef
  where
    y = testBit l 0
    {-# INLINE y #-}

insertPartialAssignment :: Lit -> PartialAssignment s -> ST s ()
insertPartialAssignment (MkLit l) (PA arr) = do
    ASSERTING(readByteArray arr (lit_to_var l) >>= \x -> assertST "insert" (x == (0xff :: Word8)))
    writeByteArray arr (lit_to_var l) (if testBit l 0 then 0x1 else 0x0 :: Word8)

deletePartialAssignment :: Lit -> PartialAssignment s -> ST s ()
deletePartialAssignment (MkLit l) (PA arr) = do
    writeByteArray arr (lit_to_var l) (0xff :: Word8)

tracePartialAssignment :: PartialAssignment s -> ST s ()
tracePartialAssignment (PA arr) = do
    n <- getSizeofMutableByteArray arr
    lits <- go n [] 0
    traceM $ "PartialAssignment " ++ show lits
  where
    go n acc i
        | i < n
        , let l = MkLit (var_to_lit i)
        = readByteArray arr i >>= \case
          0x0 -> go n (    l : acc) (i + 1)
          0x1 -> go n (neg l : acc) (i + 1)
          _   -> go n          acc  (i + 1)

        | otherwise
        = return (reverse acc)

assertLiteralInPartialAssignment :: Lit -> PartialAssignment s -> ST s ()
assertLiteralInPartialAssignment l pa =
    lookupPartialAssignment l pa >>= \case
        LTrue -> return ()
        x     -> assertST ("lit in partial: " ++ show x) False

assertLiteralUndef :: Lit -> PartialAssignment s -> ST s ()
assertLiteralUndef l pa =
    lookupPartialAssignment l pa >>= \x ->
    assertST ("assertLiteralUndef: " ++ show x) (x == LUndef)
