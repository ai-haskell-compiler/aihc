{- ORACLE_TEST xfail type signature followed by guards in where clause -}
{-# LANGUAGE GHC2021 #-}
module TypeSignatureGuards where

f = y
  where
    y :: Int
      | True = 1
