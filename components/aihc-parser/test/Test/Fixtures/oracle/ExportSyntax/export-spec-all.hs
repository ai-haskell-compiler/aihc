{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}

module Export (
  varId, ConId, (+), (:+),
  Qual.varId, Qual.ConId, (Qual.+), (Qual.:+),
  Qual.ConId(..), (Qual.:+)(..),
  ConId(UnQual, unqual, (:+) ), Qual.ConId(UnQual, unqual, (:+) ), (:+)(UnQual, unqual, (:+))
) where

varId :: Int
varId = 1

data ConId = UnQual | Other

data a :+ b = a :+: b

unqual :: Int
unqual = 2
