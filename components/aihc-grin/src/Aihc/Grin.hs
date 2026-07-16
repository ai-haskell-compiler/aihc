-- | AIHC's strict Graph Reduction Intermediate Notation dialect.
module Aihc.Grin
  ( module Aihc.Grin.Syntax,
    module Aihc.Grin.Cps,
    lowerProgram,
    lintProgram,
    GrinLintError (..),
    renderProgram,
    renderExpr,
    interpretProgramBinding,
    InterpretError (..),
    RuntimeValue (..),
  )
where

import Aihc.Grin.Cps
import Aihc.Grin.Interpret (InterpretError (..), RuntimeValue (..), interpretProgramBinding)
import Aihc.Grin.Lint (GrinLintError (..), lintProgram)
import Aihc.Grin.Lower (lowerProgram)
import Aihc.Grin.Pretty (renderExpr, renderProgram)
import Aihc.Grin.Syntax
