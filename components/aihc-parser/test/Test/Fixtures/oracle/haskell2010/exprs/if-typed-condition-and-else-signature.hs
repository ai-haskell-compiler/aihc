{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StarIsType #-}
module IfTypedConditionAndElseSignature where

a = if a :: * then a else [] :: C => '7'
