{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, TypeOperators #-}

module PromotedTypeOperatorWithStrings where

type Msg = 'Text "msg1" ':$$: 'Text "msg2"
