{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}

module ParenInfixTypeFamilyBacktick where

type family (a `And` b)
