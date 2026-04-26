{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
module LinearGADTConstructor where

data T a b c where
  MkT :: a -> b %1 -> c %1 -> T a b c
