-- | AIHC's strict Graph Reduction Intermediate Notation dialect.
module Aihc.Grin
  ( module Aihc.Grin.Syntax,
    CpsGrinProgram,
    CpsGrinError (..),
    cpsGrinProgram,
    toCpsGrin,
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
    interpretProgramFunctionSnapshot,
    InterpretError (..),
    RuntimeValue (..),
    HeapSnapshot (..),
    SnapshotValue (..),
    SnapshotCell (..),
    renderSnapshotReturn,
    renderSnapshotHeap,
    renderHeapSnapshot,
  )
where

import Aihc.Grin.Cps (CpsGrinError (..), CpsGrinProgram, cpsGrinProgram, toCpsGrin)
import Aihc.Grin.Interpret (InterpretError (..), RuntimeValue (..), interpretProgramBinding, interpretProgramFunctionSnapshot, interpretProgramIoBinding)
import Aihc.Grin.Lint (GrinLintError (..), lintProgram)
import Aihc.Grin.Lower (GrinInterface, extractGrinInterface, lowerProgram, lowerProgramWithInterface)
import Aihc.Grin.Pretty (renderExpr, renderProgram)
import Aihc.Grin.Snapshot
import Aihc.Grin.Syntax
