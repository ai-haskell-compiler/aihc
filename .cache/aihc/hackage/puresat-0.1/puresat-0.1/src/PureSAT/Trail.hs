{-# LANGUAGE CPP #-}
module PureSAT.Trail where

#define ASSERTING(x)

import Data.Primitive.PrimVar   (PrimVar, newPrimVar, readPrimVar, writePrimVar)

import PureSAT.Base
import PureSAT.Prim
import PureSAT.Clause2
import PureSAT.Level
import PureSAT.LitTable
import PureSAT.LitVar
import PureSAT.Utils

-------------------------------------------------------------------------------
-- Trail
-------------------------------------------------------------------------------

data Trail s = Trail !(PrimVar s Int) !(MutablePrimArray s Lit)

newTrail :: Int -> ST s (Trail s)
newTrail capacity = do
    size <- newPrimVar 0
    ls <- newPrimArray capacity
    return (Trail size ls)

cloneTrail :: Trail s -> ST s (Trail s)
cloneTrail (Trail size ls) = do
    capacity <- getSizeofMutablePrimArray ls
    n <- readPrimVar size
    size' <- newPrimVar n
    ls' <- newPrimArray capacity
    copyMutablePrimArray ls' 0 ls 0 n
    return (Trail size' ls')

extendTrail :: Trail s -> Int -> ST s (Trail s)
extendTrail trail@(Trail size ls) newCapacity = do
    oldCapacity <- getSizeofMutablePrimArray ls
    let capacity = nextPowerOf2 (max oldCapacity newCapacity)
    if capacity <= oldCapacity
    then return trail
    else do
        n <- readPrimVar size
        size' <- newPrimVar n
        ls' <- newPrimArray capacity
        copyMutablePrimArray ls' 0 ls 0 n
        return (Trail size' ls')

indexTrail :: Trail s -> Int -> ST s Lit
indexTrail (Trail _ ls) i = readPrimArray ls i

popTrail :: Trail s -> ST s Lit
popTrail (Trail size ls) = do
    n <- readPrimVar size
    ASSERTING(assertST "non empty trail" (n >= 1))
    writePrimVar size (n - 1)
    readPrimArray ls (n - 1)

pushTrail :: Lit -> Trail s -> ST s ()
pushTrail l (Trail size ls) = do
    n <- readPrimVar size
    writePrimVar size (n + 1)
    writePrimArray ls n l

traceTrail :: forall s. LitTable s Clause2 -> Levels s -> Trail s -> ST s ()
traceTrail reasons levels (Trail size lits) = do
    n <- readPrimVar size
    out <- go 0 n
    traceM $ unlines $ "=== Trail ===" : out
  where
    go :: Int -> Int -> ST s [String]
    go i n
        | i >= n
        = return ["=== ===== ==="]

        | otherwise
        = do
            l <- readPrimArray lits i
            Level d <- getLevel levels l
            c <- readLitTable reasons l
            ls <- go (i + 1) n
            if isNullClause c
            then return ((showChar '@' . shows d . showString " Decided " . showsPrec 11 l) "" : ls)
            else return ((showChar '@' . shows d . showString " Deduced " . showsPrec 11 l . showChar ' ' . showsPrec 11 c) "" : ls)

assertEmptyTrail :: HasCallStack => Trail s -> ST s ()
assertEmptyTrail (Trail size _) = do
    n <- readPrimVar size
    assertST "n == 0" $ n == 0
    return ()
