{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OpenInfixTypeFamilyHead where

type family a `Compose` b
