-- |
--
-- Module:      Language.Egison.Pretty.Pattern.Error
-- Description: Pretty-printing errors
-- Stability:   experimental
--
-- This module defines datatypes representing pretty-printer errors

module Language.Egison.Pretty.Pattern.Error
  ( Error(..)
  )
where


-- | A pretty printer error.
newtype Error n = UnknownInfixOperator n
  deriving (Show, Eq)
