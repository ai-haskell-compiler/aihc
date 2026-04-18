{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}

module M (T (.., P)) where

data T = A

pattern P :: T
pattern P = A
