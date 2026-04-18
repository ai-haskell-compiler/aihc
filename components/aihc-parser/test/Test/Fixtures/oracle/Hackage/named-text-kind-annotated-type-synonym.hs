{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module M where

import GHC.TypeLits (Symbol)

type NameStyle = Symbol

type UTF8 = "UTF8" :: NameStyle
