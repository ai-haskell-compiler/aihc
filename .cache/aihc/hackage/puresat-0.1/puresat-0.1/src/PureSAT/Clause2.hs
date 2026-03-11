module PureSAT.Clause2 where

import PureSAT.Base
import PureSAT.LitVar
import PureSAT.Prim

data Clause2 = MkClause2 !Bool {-# UNPACK #-} !Lit {-# UNPACK #-} !Lit {-# UNPACK #-} !(PrimArray Lit)

instance Show Clause2 where
    showsPrec d (MkClause2 flags l1 l2 ls) = showParen (d > 11) $ showString "cl " . shows flags . shows (l1 : l2 : primArrayToList ls)

litInClause :: Lit -> Clause2 -> Bool
litInClause l (MkClause2 _ l1 l2 ls) =
    l == l1 || l == l2 || foldrPrimArray (\l' next -> l' == l || next) False ls

isBinaryClause2 :: Clause2 -> Bool
isBinaryClause2 (MkClause2 _ _ _ ls) = sizeofPrimArray ls == 0

forLitInClause2_ :: Clause2 -> (Lit -> ST s ()) -> ST s ()
forLitInClause2_ (MkClause2 _ l1 l2 ls) f = do
    f l1
    f l2
    -- manually inlines traversePrimArray_
    forLitInClause2Go 0
  where
    !sz = sizeofPrimArray ls
    forLitInClause2Go !i = when (i < sz) $ f (indexPrimArray ls i) >> forLitInClause2Go (i + 1)
{-# INLINE forLitInClause2_ #-}

nullClause :: Clause2
nullClause = MkClause2 False (MkLit 1) (MkLit 1) emptyPrimArray

isNullClause :: Clause2 -> Bool
isNullClause (MkClause2 _ l1 l2 _) = l1 == l2

sizeofClause2 :: Clause2 -> Int
sizeofClause2 (MkClause2 _ _ _ ls) = sizeofPrimArray ls + 2
