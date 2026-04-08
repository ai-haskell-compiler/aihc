{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module InfixTypeFamilyHead where

type family l `And` r where
  l `And` r = l
