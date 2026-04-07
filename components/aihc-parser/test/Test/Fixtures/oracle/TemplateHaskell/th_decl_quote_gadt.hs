{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module TH_Decl_Quote_GADT where

import Language.Haskell.TH (Dec, Q)

decl :: Q [Dec]
decl = [d|
  data Expr a where
    Lit :: Int -> Expr Int
    Add :: Expr Int -> Expr Int -> Expr Int
  |]
