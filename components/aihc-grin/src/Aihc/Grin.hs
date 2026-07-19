-- | AIHC's strict Graph Reduction Intermediate Notation dialect.
module Aihc.Grin
  ( module Aihc.Grin.Syntax,
    CpsGrinProgram,
    CpsGrinError (..),
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
    toCpsGrin,
    GcGrinProgram,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    gcUpdateFunction,
    lowerGc,
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

import Aihc.Grin.Cps
  ( CpsGrinError (..),
    CpsGrinProgram,
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
    toCpsGrin,
  )
import Aihc.Grin.Gc (GcGrinProgram, gcContinuationFunctions, gcFunctionContinuations, gcGrinProgram, gcUpdateFunction, lowerGc)
import Aihc.Grin.Interpret (InterpretError (..), RuntimeValue (..), interpretProgramBinding, interpretProgramFunctionSnapshot, interpretProgramIoBinding)
import Aihc.Grin.Lint (GrinLintError (..), lintProgram)
import Aihc.Grin.Lower (GrinInterface, extractGrinInterface, lowerProgram, lowerProgramWithInterface)
import Aihc.Grin.Pretty (renderExpr, renderProgram)
import Aihc.Grin.Snapshot
import Aihc.Grin.Syntax
