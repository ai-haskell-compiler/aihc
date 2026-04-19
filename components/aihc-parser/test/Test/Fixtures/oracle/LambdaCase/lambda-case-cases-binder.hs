{- ORACLE_TEST pass -}
{-# LANGUAGE LambdaCase #-}

module LambdaCaseCasesBinder where

identityCases :: a -> a
identityCases = \cases -> cases
