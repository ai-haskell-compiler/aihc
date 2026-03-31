{- ORACLE_TEST xfail GADTs with implicit params -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
module GADTsImplicitParams where

data T where
  MkT :: (?f :: Int) => T
