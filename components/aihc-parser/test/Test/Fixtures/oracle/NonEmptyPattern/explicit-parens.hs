{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module ExplicitParens where

import Data.List.NonEmpty (NonEmpty(..))

f ((x :| y) : ys) = undefined
f _ = undefined
