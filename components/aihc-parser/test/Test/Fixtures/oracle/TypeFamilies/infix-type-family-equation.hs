{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module InfixTypeFamilyEquation where

type family a ** b where
  a ** b = ()
