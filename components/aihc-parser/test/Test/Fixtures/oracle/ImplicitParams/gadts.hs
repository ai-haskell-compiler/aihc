{- ORACLE_TEST pass -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
module GADTsImplicitParams where

data T where
  MkT :: (?f :: Int) => T
