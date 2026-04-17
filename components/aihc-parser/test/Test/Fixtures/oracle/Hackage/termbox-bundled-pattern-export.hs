{- ORACLE_TEST xfail reason="bundled pattern synonym export after .. is rejected" -}
{-# LANGUAGE PatternSynonyms #-}

module M (T (.., P)) where

data T = T

pattern P :: T
pattern P = T
