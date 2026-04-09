{- ORACLE_TEST xfail promoted type operator with string literals -}
{-# LANGUAGE DataKinds, TypeOperators #-}

module PromotedTypeOperatorWithStrings where

type Msg = 'Text "msg1" ':$$: 'Text "msg2"
