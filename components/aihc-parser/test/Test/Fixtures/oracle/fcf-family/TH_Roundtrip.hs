{-# LANGUAGE GHC2021 #-}

-- Minimal snippet reproducing GHC pretty-printer parentheses difference

-- The original roundtrip mismatch referenced a TyVarSig printing change
-- We reproduce a small AST-ish declaration that causes pretty-printer output

type family GetRes a

-- A declaration that mentions TyVarSig-like shape
class C where
  type GetRes a
