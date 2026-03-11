{- | Logical predicates.

Pretty predicates use infix propositional operators. This is chosen for
consistency. I'm not really sure which I prefer over all, perhaps best would be
to mix and match. Please let the maintainers know if you have a better idea.
-}

module Rerefined.Predicate.Logical
  ( And, Iff, If, Nand, Nor, Not, Or, Xor
  ) where

import Rerefined.Predicate.Logical.And
import Rerefined.Predicate.Logical.Iff
import Rerefined.Predicate.Logical.If
import Rerefined.Predicate.Logical.Nand
import Rerefined.Predicate.Logical.Nor
import Rerefined.Predicate.Logical.Not
import Rerefined.Predicate.Logical.Or
import Rerefined.Predicate.Logical.Xor
