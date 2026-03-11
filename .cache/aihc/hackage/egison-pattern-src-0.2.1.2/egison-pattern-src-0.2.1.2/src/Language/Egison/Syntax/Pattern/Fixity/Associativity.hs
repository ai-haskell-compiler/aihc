-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Fixity.Associativity
-- Description: Associativity of infix operators
-- Stability:   experimental
--
-- An associativity of infix operators.

module Language.Egison.Syntax.Pattern.Fixity.Associativity
  ( Associativity(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data
                                                , Typeable
                                                )


-- | Associativity of infix operators.
data Associativity = AssocLeft | AssocRight | AssocNone
  deriving (Show, Eq, Generic, Data, Typeable)
