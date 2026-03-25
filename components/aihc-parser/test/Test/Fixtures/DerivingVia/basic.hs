{-# LANGUAGE DerivingVia #-}
module Basic where

newtype MyInt = MyInt Int
  deriving Show via Int
