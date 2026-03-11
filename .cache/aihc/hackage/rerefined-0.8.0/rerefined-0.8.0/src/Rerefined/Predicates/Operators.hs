{- | Predicate re-exports using operators.

We stick with non-operators for base predicate definitions because, very simply,
there aren't enough symbols to go around. The operators we would want to use for
logical predicates are already being used on the term level, and I forsee a
future full of promoted functions, so I wouldn't want to take up the type-level
name.

Instead, I'm providing operators in separate modules. It's not ideal, because
predicates already have operators defined in their pretty printing, and they may
not coincide with available Haskell symbols. I'm not really sure how to do all
this, it's very tentative for now.
-}

module Rerefined.Predicates.Operators
  (
  -- * Logical
  -- $logical
    type (.&&), type (.||)
  , type (.<->), type (.->)
  , type (.!&), type (.!|)
  , type (.!), type (.!=)

  -- * Relational
  , type (.<), type (.<=), type (.==), type (./=), type (.>=), type (.>)
  ) where

import Rerefined.Predicate.Logical
import Rerefined.Predicate.Relational.Internal

{- $logical

Awkward mix of Boolean and propositional operators. Sorry. Let me know if you
have a better idea.
-}

infixr 3 .&&
type (.&&) = And

infixr 2 .||
type (.||) = Or

infixr 3 .!&
type (.!&) = Nand

infix 9 .!
type (.!) = Not

infixr 2 .!|
type (.!|) = Nor

infixr 4 .!=
type (.!=) = Xor

infixr 4 .<->
type (.<->) = Iff

infixr 4 .->
type (.->) = If

infix 4 .<, .<=, .==, ./=, .>=, .>
type (.<)  = RelOpLT
type (.<=) = RelOpLTE
type (.==) = RelOpEQ
type (./=) = RelOpNEQ
type (.>=) = RelOpGTE
type (.>)  = RelOpGT
