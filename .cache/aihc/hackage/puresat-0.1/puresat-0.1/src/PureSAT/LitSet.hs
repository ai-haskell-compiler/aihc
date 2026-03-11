{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module PureSAT.LitSet where

#define ASSERTING(x)

import Data.Primitive.PrimVar (readPrimVar)

import PureSAT.Base
import PureSAT.Clause2
import PureSAT.LitVar
import PureSAT.Prim
import PureSAT.SparseSet

-------------------------------------------------------------------------------
-- LitSet
-------------------------------------------------------------------------------

newtype LitSet s = LS (SparseSet s)

indexLitSet :: forall s. LitSet s -> Int -> ST s Lit
indexLitSet (LS xs) i = coerce (indexSparseSet @s xs i)

newLitSet :: Int -> ST s (LitSet s)
newLitSet n = LS <$> newSparseSet n

insertLitSet :: Lit -> LitSet s -> ST s ()
insertLitSet (MkLit l) (LS ls) = insertSparseSet ls l

deleteLitSet :: Lit -> LitSet s -> ST s ()
deleteLitSet (MkLit l) (LS ls) = deleteSparseSet ls l

{-# INLINE minViewLitSet #-}
minViewLitSet :: LitSet s -> ST s r -> (Lit -> ST s r) -> ST s r
minViewLitSet (LS xs) no yes = popSparseSet_ xs no (coerce yes)

clearLitSet :: LitSet s -> ST s ()
clearLitSet (LS xs) = clearSparseSet xs

elemsLitSet :: LitSet s -> ST s [Lit]
elemsLitSet (LS s) = coerce (elemsSparseSet s)

memberLitSet :: LitSet s -> Lit -> ST s Bool
memberLitSet (LS xs) (MkLit x) = memberSparseSet xs x

sizeofLitSet :: LitSet s -> ST s Int
sizeofLitSet (LS xs) = sizeofSparseSet xs

unsingletonLitSet :: LitSet s -> ST s Lit
unsingletonLitSet (LS SS {..}) = do
    -- ASSERTING(n <- readPrimVar size)
    ASSERTING(assertST "size == 1" (n == 1))
    x <- readPrimArray dense 0
    return (MkLit x)

litSetToClause :: LitSet s -> ST s Clause2
litSetToClause (LS SS {..}) = do
    n <- readPrimVar size
    ASSERTING(assertST "size >= 2" (n >= 2))
    l1 <- readPrimArray dense 0
    l2 <- readPrimArray dense 1
    ls <- freezePrimArray dense 2 (n - 2)
    -- TODO: learned clauses only
    return $! MkClause2 True (coerce l1) (coerce l2) (coercePrimArrayLit ls)
