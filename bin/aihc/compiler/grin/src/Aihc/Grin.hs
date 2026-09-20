-- | AIHC's strict Graph Reduction Intermediate Notation dialect.
module Aihc.Grin
  ( module Aihc.Grin.Syntax,
    normalizeGrinProgram,
    normalizeGrinExpr,
    CpsGrinProgram,
    CpsGrinError (..),
    ContinuationFrameKind (..),
    continuationFrameKindCode,
    cpsContinuationFrames,
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    toCpsGrin,
    GcGrinProgram,
    entryGcProgram,
    gcContinuationFrames,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    lowerGc,
    lowerProgram,
    lintProgram,
    lintCpsProgram,
    lintGcProgram,
    GrinLintError (..),
    GrinParseError,
    parseProgram,
    parseExpr,
    renderParseError,
    prettyProgram,
    ProgramStreams (..),
    interpretProgramBinding,
    interpretProgramIoBinding,
    InterpretError (..),
    RuntimeValue (..),
  )
where

import Aihc.Grin.Anf (normalizeGrinExpr, normalizeGrinProgram)
import Aihc.Grin.Cps
  ( ContinuationFrameKind (..),
    CpsGrinError (..),
    CpsGrinProgram,
    continuationFrameKindCode,
    cpsContinuationFrames,
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    toCpsGrin,
  )
import Aihc.Grin.Gc (GcGrinProgram, entryGcProgram, gcContinuationFrames, gcContinuationFunctions, gcFunctionContinuations, gcGrinProgram, lowerGc)
import Aihc.Grin.Interpret (InterpretError (..), ProgramStreams (..), RuntimeValue (..), interpretProgramBinding, interpretProgramIoBinding)
import Aihc.Grin.Lint (GrinLintError (..), lintCpsProgram, lintGcProgram, lintProgram)
import Aihc.Grin.Lower (lowerProgram)
import Aihc.Grin.Parser (GrinParseError, parseExpr, parseProgram, renderParseError)
import Aihc.Grin.Pretty (prettyProgram)
import Aihc.Grin.Syntax
