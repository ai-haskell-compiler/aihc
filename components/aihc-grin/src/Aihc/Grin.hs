-- | AIHC's strict Graph Reduction Intermediate Notation dialect.
module Aihc.Grin
  ( module Aihc.Grin.Syntax,
    CpsGrinProgram,
    CpsGrinError (..),
    ContinuationFrameKind (..),
    continuationFrameKindCode,
    cpsContinuationFrames,
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
    toCpsGrin,
    GcGrinProgram,
    gcContinuationFrames,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    gcUpdateFunction,
    lowerGc,
    lowerProgram,
    GrinLinkNames,
    renderLinkName,
    linkNamesForProgram,
    lowerProgramWithLinkNames,
    GrinInterface,
    extractGrinInterface,
    extractGrinInterfaceWithLinkNames,
    lowerProgramWithInterface,
    lowerProgramWithInterfaceAndLinkNames,
    lintProgram,
    GrinLintError (..),
    GrinParseError,
    parseProgram,
    parseExpr,
    renderParseError,
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
  ( ContinuationFrameKind (..),
    CpsGrinError (..),
    CpsGrinProgram,
    continuationFrameKindCode,
    cpsContinuationFrames,
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
    toCpsGrin,
  )
import Aihc.Grin.Gc (GcGrinProgram, gcContinuationFrames, gcContinuationFunctions, gcFunctionContinuations, gcGrinProgram, gcUpdateFunction, lowerGc)
import Aihc.Grin.Interpret (InterpretError (..), RuntimeValue (..), interpretProgramBinding, interpretProgramFunctionSnapshot, interpretProgramIoBinding)
import Aihc.Grin.Lint (GrinLintError (..), lintProgram)
import Aihc.Grin.Lower
  ( GrinInterface,
    GrinLinkNames,
    extractGrinInterface,
    extractGrinInterfaceWithLinkNames,
    linkNamesForProgram,
    lowerProgram,
    lowerProgramWithInterface,
    lowerProgramWithInterfaceAndLinkNames,
    lowerProgramWithLinkNames,
    renderLinkName,
  )
import Aihc.Grin.Parser (GrinParseError, parseExpr, parseProgram, renderParseError)
import Aihc.Grin.Pretty (renderExpr, renderProgram)
import Aihc.Grin.Snapshot
import Aihc.Grin.Syntax
