{- ORACLE_TEST xfail closed type family -}
{-# LANGUAGE TypeFamilies #-}
module ClosedTypeFamily where

type family F a where
  F Int = String
  F a = a