{-# LANGUAGE GADTSyntax #-}

module GadtNewtype where

newtype Down a where
  Down :: a -> Down a
