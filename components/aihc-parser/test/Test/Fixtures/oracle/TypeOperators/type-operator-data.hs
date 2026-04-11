{- ORACLE_TEST xfail reason="data declaration with type operator symbol :+: in parentheses not parsed correctly" -}
{-# LANGUAGE GHC2021 #-}

module TypeOperatorData where

data (f :+: g) e = Left' (f e) | Right' (g e)
