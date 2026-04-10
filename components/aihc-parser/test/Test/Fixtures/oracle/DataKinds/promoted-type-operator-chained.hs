{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, TypeOperators #-}

module PromotedTypeOperatorChained where

-- Multiple promoted type operators chained together
type Chain = 'Text "a" ':$$: 'Text "b" ':$$: 'Text "c"
