{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module InfixTypeFamilyInstance where

type family a `And` b

type instance a `And` b = a
