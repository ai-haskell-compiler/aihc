{-# LANGUAGE DerivingVia #-}
module MultipleClasses where

newtype MyInt = MyInt Int
  deriving (Eq, Ord) via Int
