{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module ClosedTypeFamily where

type family F a where
  F Int = String
  F a = a