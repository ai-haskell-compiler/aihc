{-# LANGUAGE GADTSyntax #-}

module GadtMultiCon where

data T a where
  T1, T2 :: a -> T a
  T3 :: T a
