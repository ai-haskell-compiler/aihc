{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ParenInfixTypeFamilyTailParams where

type family (a ++ b) c
