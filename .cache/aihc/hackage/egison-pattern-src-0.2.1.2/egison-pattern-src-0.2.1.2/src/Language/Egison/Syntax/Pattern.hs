-- |
--
-- Module:      Language.Egison.Syntax.Pattern
-- Description: Manipulating Egison patterns: abstract syntax, parser, and pretty-printer
-- Stability:   experimental
--
-- This package provides a standalone syntax definition for patterns in [the Egison programming language](https://www.egison.org/).

module Language.Egison.Syntax.Pattern
  ( module X
  )
where

import           Language.Egison.Syntax.Pattern.Expr
                                               as X
import           Language.Egison.Syntax.Pattern.Base
                                               as X
import           Language.Egison.Syntax.Pattern.Combinator
                                               as X
