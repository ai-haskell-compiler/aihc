{-# LANGUAGE LambdaCase #-}
module PureSAT.Satisfied where

import PureSAT.Base
import PureSAT.LitVar
import PureSAT.Clause2
import PureSAT.LBool
import PureSAT.PartialAssignment
import PureSAT.Prim

data Satisfied_
    = Satisfied_
    | Conflicting_
    | Unit_ !Lit
    | Unresolved_ !Lit !Lit
  deriving Show

{-# INLINE satisfied2_ #-}
satisfied2_ :: PartialAssignment s -> Clause2 -> (Satisfied_ -> ST s r) -> ST s r
satisfied2_ !pa !(MkClause2 _ l1 l2 ls) kont = go0
  where
    !len = sizeofPrimArray ls

    -- initial state
    go0 = lookupPartialAssignment l1 pa >>= \case
        LUndef -> go1
        LTrue  -> kont Satisfied_
        LFalse -> go2

    -- l1 -> Undef
    go1 = lookupPartialAssignment l2 pa >>= \case
        LUndef -> goTwo l1 l2 0
        LTrue  -> kont Satisfied_
        LFalse -> goOne l1 0

    -- l1 -> False
    go2 = lookupPartialAssignment l2 pa >>= \case
        LUndef -> goOne l2 0
        LTrue  -> kont Satisfied_
        LFalse -> goNone 0

    goNone !i
        | i >= len
        = kont Conflicting_

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goOne l (i + 1)
            LTrue  -> kont Satisfied_
            LFalse -> goNone (i + 1)

    goOne !k1 !i
        | i >= len
        = kont $! Unit_ k1

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goTwo k1 l (i + 1)
            LTrue  -> kont Satisfied_
            LFalse -> goOne k1 (i + 1)

    goTwo !k1 !k2 !i
        | i >= len
        = kont $! Unresolved_ k1 k2

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goTwo k1 k2 (i + 1)
            LTrue  -> kont Satisfied_
            LFalse -> goTwo k1 k2 (i + 1)
