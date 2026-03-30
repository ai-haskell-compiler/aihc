{- ORACLE_TEST
id: gadts-basic
category: declarations
expected: pass
-}
{-# LANGUAGE GADTs #-}

module GADTsBasic where

data Term a where
  TInt :: Int -> Term Int
  TBool :: Bool -> Term Bool
