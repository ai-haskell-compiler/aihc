-- | AIHC's strict Graph Reduction Intermediate Notation dialect.
module Aihc.Grin
  ( module Aihc.Grin.Syntax,
    lowerProgram,
    GrinInterface,
    extractGrinInterface,
    lowerProgramWithInterface,
    lintProgram,
    GrinLintError (..),
    renderProgram,
    renderExpr,
    interpretProgramBinding,
    interpretProgramIoBinding,
    InterpretError (..),
    RuntimeValue (..),
  )
where

import Aihc.Grin.Interpret (InterpretError (..), RuntimeValue (..), interpretProgramBinding, interpretProgramIoBinding)
import Aihc.Grin.Lint (GrinLintError (..), lintProgram)
import Aihc.Grin.Lower (GrinInterface, extractGrinInterface, lowerProgram, lowerProgramWithInterface)
import Aihc.Grin.Pretty (renderExpr, renderProgram)
import Aihc.Grin.Syntax
