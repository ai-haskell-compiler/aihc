{-# LANGUAGE CPP #-}
module PureSAT.Prim (
    -- * ByteArray
    P.MutableByteArray,
    P.newByteArray,
    P.getSizeofMutableByteArray,
    readByteArray,
    writeByteArray,
    shrinkMutableByteArray,
    fillByteArray,
    copyMutableByteArray,
    resizeMutableByteArray,
    -- * Array of primitive values
    P.Prim,
    P.PrimArray (..),
    P.MutablePrimArray (..),
    P.newPrimArray,
    P.getSizeofMutablePrimArray,
    P.resizeMutablePrimArray,
    P.primArrayFromList,
    P.primArrayToList,
    P.foldrPrimArray,
    readPrimArray,
    writePrimArray,
    setPrimArray,
    indexPrimArray,
    P.sizeofPrimArray,
    freezePrimArray,
    P.emptyPrimArray,
    copyMutablePrimArray,
    -- * Array
    P.MutableArray,
    P.newArray,
    P.sizeofMutableArray,
    readArray,
    writeArray,
    copyMutableArray,
) where

#define PureSAT_PRIM_BOUNDS_CHECK

#ifdef PureSAT_PRIM_BOUNDS_CHECK
#define BOUNDS_CHECK_CTX HasCallStack =>
#else
#define BOUNDS_CHECK_CTX HasCallStack
#endif

import qualified Data.Primitive as P

import PureSAT.Base

-------------------------------------------------------------------------------
-- ByteArray
-------------------------------------------------------------------------------


readByteArray :: BOUNDS_CHECK_CTX P.MutableByteArray s -> Int -> ST s Word8
readByteArray arr i = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutableByteArray arr
    assertST "readByteArray" $ 0 <= i && i < n
#endif
    P.readByteArray arr i

writeByteArray :: BOUNDS_CHECK_CTX P.MutableByteArray s -> Int -> Word8 -> ST s ()
writeByteArray arr i x = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutableByteArray arr
    assertST "readByteArray" $ 0 <= i && i < n
#endif
    P.writeByteArray arr i x

shrinkMutableByteArray :: BOUNDS_CHECK_CTX P.MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray arr m = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutableByteArray arr
    assertST "shrinkMutableByteArray" $ 0 <= m && m <= n
#endif
    P.shrinkMutableByteArray arr m

fillByteArray :: BOUNDS_CHECK_CTX P.MutableByteArray s -> Int -> Int -> Word8 -> ST s ()
fillByteArray arr off len x = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutableByteArray arr
    -- traceM $ "fillByteArray " ++ show (off, len, n)
    assertST "fillByteArray" $ 0 <= off && off < n
    assertST "fillByteArray" $ 0 <= (off + len) && (off + len) <= n 
#endif
    P.fillByteArray arr off len x

copyMutableByteArray :: BOUNDS_CHECK_CTX P.MutableByteArray s -> Int -> P.MutableByteArray s -> Int -> Int -> ST s ()
copyMutableByteArray dst off src off' len = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutableByteArray dst
    assertST "copyMutableByteArray" $ 0 <= off && off < n
    assertST "copyMutableByteArray" $ 0 <= (off + len) && (off + len) <= n 

    m <- P.getSizeofMutableByteArray src
    assertST "copyMutableByteArray" $ 0 <= off' && off' < m
    assertST "copyMutableByteArray" $ 0 <= (off' + len) && (off' + len) <= m 
#endif
    P.copyMutableByteArray dst off src off' len

resizeMutableByteArray :: BOUNDS_CHECK_CTX P.MutableByteArray s -> Int -> ST s (P.MutableByteArray s)
resizeMutableByteArray arr len = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
#endif
    P.resizeMutableByteArray arr len

-------------------------------------------------------------------------------
-- PrimArray
-------------------------------------------------------------------------------

readPrimArray :: BOUNDS_CHECK_CTX P.Prim a => P.MutablePrimArray s a -> Int -> ST s a
readPrimArray arr i =  do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutablePrimArray arr
    assertST "readPrimArray" $ 0 <= i && i < n
#endif
    P.readPrimArray arr i

writePrimArray :: BOUNDS_CHECK_CTX P.Prim a => P.MutablePrimArray s a -> Int -> a -> ST s ()
writePrimArray arr i x = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutablePrimArray arr
    assertST "writePrimArray" $ 0 <= i && i < n
#endif
    P.writePrimArray arr i x

setPrimArray :: BOUNDS_CHECK_CTX P.Prim a => P.MutablePrimArray s a -> Int -> Int -> a -> ST s ()
setPrimArray arr off len x = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutablePrimArray arr
    assertST "setPrimArray" $ 0 <= off && off < n
    assertST "setPrimArray" $ 0 <= (off + len) && (off + len) <= n 
#endif
    P.setPrimArray arr off len x

freezePrimArray :: BOUNDS_CHECK_CTX P.Prim a => P.MutablePrimArray s a -> Int -> Int -> ST s (P.PrimArray a)
freezePrimArray arr off len = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutablePrimArray arr
    assertST "setPrimArray" $ 0 <= off && off < n
    assertST "setPrimArray" $ 0 <= (off + len) && (off + len) <= n 
#endif
    P.freezePrimArray arr off len

indexPrimArray :: BOUNDS_CHECK_CTX P.Prim a => P.PrimArray a -> Int -> a
indexPrimArray arr i
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    | not (0 <= i && i < P.sizeofPrimArray arr) = error "indexPrimArray"
#endif
    | otherwise = P.indexPrimArray arr i

copyMutablePrimArray :: BOUNDS_CHECK_CTX P.Prim a => P.MutablePrimArray s a -> Int -> P.MutablePrimArray s a -> Int -> Int -> ST s ()
copyMutablePrimArray dst off src off' len = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    n <- P.getSizeofMutablePrimArray dst
    assertST "copyMutablePrimArray" $ 0 <= off && off < n
    assertST "copyMutablePrimArray" $ 0 <= (off + len) && (off + len) <= n 

    m <- P.getSizeofMutablePrimArray src
    assertST "copyMutablePrimArray" $ 0 <= off' && off' < m
    assertST "copyMutablePrimArray" $ 0 <= (off' + len) && (off' + len) <= m 
#endif
    P.copyMutablePrimArray dst off src off' len

-------------------------------------------------------------------------------
-- Array
-------------------------------------------------------------------------------

readArray :: BOUNDS_CHECK_CTX P.MutableArray s a -> Int -> ST s a
readArray arr i = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    assertST "readArray" $ 0 <= i && i < P.sizeofMutableArray arr
#endif
    P.readArray arr i

writeArray :: BOUNDS_CHECK_CTX P.MutableArray s a -> Int -> a -> ST s ()
writeArray arr i x = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    assertST "readArray" $ 0 <= i && i < P.sizeofMutableArray arr
#endif
    P.writeArray arr i x

copyMutableArray :: BOUNDS_CHECK_CTX P.MutableArray s a -> Int -> P.MutableArray s a -> Int -> Int -> ST s ()
copyMutableArray dst off src off' len = do
#ifdef PureSAT_PRIM_BOUNDS_CHECK
    let n = P.sizeofMutableArray dst
    assertST "copyMutableArray" $ 0 <= off && off < n
    assertST "copyMutableArray" $ 0 <= (off + len) && (off + len) <= n 

    let m = P.sizeofMutableArray src
    assertST "copyMutableArray" $ 0 <= off' && off' < m
    assertST "copyMutableArray" $ 0 <= (off' + len) && (off' + len) <= m 
#endif
    P.copyMutableArray dst off src off' len
