{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module Standalone where

newtype MyInt = MyInt Int
deriving via Int instance Show MyInt
