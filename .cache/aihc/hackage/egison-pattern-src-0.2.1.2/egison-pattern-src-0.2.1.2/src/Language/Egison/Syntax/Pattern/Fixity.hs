-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Fixity
-- Description: Fixity of infix operators
-- Stability:   experimental
--
-- A fixity of infix operators.

module Language.Egison.Syntax.Pattern.Fixity
  ( Fixity(..)
  , Associativity(..)
  , Precedence(..)
  )
where

import           Language.Egison.Syntax.Pattern.Fixity.Associativity
                                                ( Associativity(..) )
import           Language.Egison.Syntax.Pattern.Fixity.Precedence
                                                ( Precedence(..) )


-- | Fixity of infix operators.
data Fixity n =
  Fixity { associativity :: Associativity
         , precedence :: Precedence
         , symbol :: n
         }
