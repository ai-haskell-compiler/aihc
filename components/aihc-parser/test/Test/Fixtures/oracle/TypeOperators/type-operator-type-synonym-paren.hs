{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}

module TypeOperatorTypeSynonymParen where

type (:+:) = (,)

usePlus :: Int :+: Int
usePlus = (1, 2)
