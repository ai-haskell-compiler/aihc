{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}

module TypeLevelSubtractionSynonym where

type a - b = Either a b

f :: Int - String
f = undefined
