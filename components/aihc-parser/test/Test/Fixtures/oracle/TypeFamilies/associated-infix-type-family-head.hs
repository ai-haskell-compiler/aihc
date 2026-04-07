{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AssociatedInfixTypeFamilyHead where

class Combines a b where
  type a `And` b :: *
