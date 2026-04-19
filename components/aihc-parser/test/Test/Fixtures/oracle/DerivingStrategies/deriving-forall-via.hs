{- ORACLE_TEST pass -}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DerivingForallVia where

class C a b c

newtype T a = T a
  deriving (C a Int Bool) via (Either a Int)
