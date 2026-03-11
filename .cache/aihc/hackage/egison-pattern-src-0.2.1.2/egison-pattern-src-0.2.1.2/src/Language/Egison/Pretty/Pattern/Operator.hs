-- |
--
-- Module:      Language.Egison.Pretty.Pattern.Operator
-- Description: Infix operator datatypes
-- Stability:   experimental
--
-- A datatypes for infix operators.

module Language.Egison.Pretty.Pattern.Operator
  ( Operator(..)
  )
where

import           Data.Text                      ( Text )

import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Precedence
                                                , Associativity
                                                )


data Operator
  = InfixOp  { associativity :: Associativity
             , precedence :: Precedence
             , symbol :: Text
             }
  | PrefixOp { precedence :: Precedence
             , symbol :: Text
             }
