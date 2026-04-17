{- ORACLE_TEST xfail reason="export lists reject pattern synonym names after .. in a constructor export" -}
{-# LANGUAGE PatternSynonyms #-}

module M (T (.., P)) where

data T = A

pattern P :: T
pattern P = A
