-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Base
-- Description: Abstract syntax tree for Egison pattern expression
-- Stability:   experimental
--
-- This module defines an AST (Abstract Syntax Tree) for Egison pattern expression.

module Language.Egison.Syntax.Pattern.Expr
  ( Expr(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data
                                                , Typeable
                                                )


-- | Egison pattern expressions.
-- @n@ is a type for name references in patterns, such as them in pattern constructors.
-- @v@ is a type for name bindings in patterns, such as them in pattern variables.
-- @e@ is a type for expressions in patterns, such as them in value patterns.
data Expr n v e
  -- | Wildcard pattern. Match with everything.
  = Wildcard
  -- | Pattern variable. The matched term is bound to the name.
  | Variable v
  -- | Value pattern. Match with equal terms.
  | Value e
  -- | Predicate pattern. Match with terms that satisfy the given predicate.
  | Predicate e
  -- | And pattern. Match when both patterns matches.
  | And (Expr n v e) (Expr n v e)
  -- | Or pattern. Match when one of given patterns matches.
  | Or (Expr n v e) (Expr n v e)
  -- | Not pattern. Match when the given pattern does not match.
  | Not (Expr n v e)
  -- | Tuple pattern. Match with tuples whose elements match with respective patterns.
  | Tuple [Expr n v e]
  -- | Collection pattern. Match with collections whose elements match with respective patterns.
  | Collection [Expr n v e]
  -- | User-defined infix pattern.
  | Infix n (Expr n v e) (Expr n v e)
  -- | User-defined normal pattern.
  | Pattern n [Expr n v e]
  deriving (Eq, Ord, Show, Typeable, Data, Generic)
