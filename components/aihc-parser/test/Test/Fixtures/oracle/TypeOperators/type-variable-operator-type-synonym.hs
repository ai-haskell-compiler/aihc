{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}

module TypeVariableOperatorTypeSynonym where

type f ~> g = f -> g
