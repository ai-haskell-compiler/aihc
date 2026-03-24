{-# LANGUAGE GADTSyntax #-}

module GadtInfix where

infix 6 :--:
data T a where
  (:--:) :: Int -> Bool -> T Int
