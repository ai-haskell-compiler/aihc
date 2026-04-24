{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module TypeSignatureGuards where

f = y
  where
    y :: Int
      | True = 1
