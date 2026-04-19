{- ORACLE_TEST pass -}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DerivingContextVia where

class C a b

newtype T a = T a
  deriving (C a Int) via (Maybe a)
