{- ORACLE_TEST
id: gadt-infix
category: declarations
expected: pass
-}
{-# LANGUAGE GADTSyntax #-}

module GadtInfix where

infix 6 :--:
data T a where
  (:--:) :: Int -> Bool -> T Int
