{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}

module InfixBranchTypeSignature where

x = if | True -> a
  + b :: Int
