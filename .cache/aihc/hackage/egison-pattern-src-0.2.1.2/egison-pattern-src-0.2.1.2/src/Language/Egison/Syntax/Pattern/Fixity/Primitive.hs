-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Fixity.Primitive
-- Description: Fixity of primitive operators
-- Stability:   experimental
--
-- This internal module defines fixities of primitive operators.

module Language.Egison.Syntax.Pattern.Fixity.Primitive
  ( andPrecedence
  , andAssociativity
  , orPrecedence
  , orAssociativity
  )
where

import           Language.Egison.Syntax.Pattern.Fixity.Precedence
                                                ( Precedence(..) )
import           Language.Egison.Syntax.Pattern.Fixity.Associativity
                                                ( Associativity(..) )


andPrecedence :: Precedence
andPrecedence = Precedence 3

orPrecedence :: Precedence
orPrecedence = Precedence 2

andAssociativity :: Associativity
andAssociativity = AssocRight

orAssociativity :: Associativity
orAssociativity = AssocRight
