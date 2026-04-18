{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}

module M (T (.., P)) where

data T = T

pattern P :: T
pattern P = T
