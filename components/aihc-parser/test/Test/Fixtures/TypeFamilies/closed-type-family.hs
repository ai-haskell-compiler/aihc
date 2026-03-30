{- ORACLE_TEST
id: closed-type-family
category: declarations
expected: xfail
reason: closed type family
-}
{-# LANGUAGE TypeFamilies #-}
module ClosedTypeFamily where

type family F a where
  F Int = String
  F a = a
