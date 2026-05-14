{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module PromotedTupleLeadingChar where

import GHC.TypeLits (Symbol)

type family DropHash (value :: Maybe (Char, Symbol)) :: Symbol where
  DropHash ('Just '( '#', rest)) = rest
  DropHash 'Nothing = ""
