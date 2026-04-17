{- ORACLE_TEST xfail reason="kind-annotated type synonym rhs is rejected after the promoted literal" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module M where

import GHC.TypeLits (Symbol)

type NameStyle = Symbol

type UTF8 = "UTF8" :: NameStyle
