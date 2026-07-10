{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module VariableLeadingInfixDeclarations where

a@_ + _ = []
b :+ _ = []
