{- ORACLE_TEST
id: gadt-strict
category: declarations
expected: pass
-}
{-# LANGUAGE GADTSyntax #-}

module GadtStrict where

data Term a where
    Lit    :: !Int -> Term Int
    If     :: Term Bool -> !(Term a) -> !(Term a) -> Term a
    Pair   :: Term a -> Term b -> Term (a, b)
