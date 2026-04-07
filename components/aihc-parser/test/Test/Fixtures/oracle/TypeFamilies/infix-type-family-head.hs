{- ORACLE_TEST xfail infix-type-family-head parser support pending for infix type family head syntax -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module InfixTypeFamilyHead where

type family l `And` r where
  l `And` r = l
