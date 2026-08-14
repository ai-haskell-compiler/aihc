-- | System FC core language with desugaring and lint.
--
-- This module re-exports the key types and functions for working with
-- System FC Core:
--
-- * 'Aihc.Fc.Syntax' — Core grammar (expressions, bindings, alternatives)
-- * 'Aihc.Fc.Pretty' — Unicode pretty-printer
-- * 'Aihc.Fc.Lint' — Structural type checker for Core
-- * 'Aihc.Fc.Desugar' — Translation from TC-annotated surface AST to Core
module Aihc.Fc
  ( -- * Syntax
    module Aihc.Fc.Syntax,

    -- * Pretty-printing
    renderProgram,
    renderExpr,
    renderType,
    renderTopBind,
    FcParseError,
    parseProgram,
    parseExpr,
    parseType,
    renderParseError,

    -- * Evaluation
    evalProgramBinding,
    evalExpr,
    renderValue,
    renderRawValue,
    EvalError (..),
    Value (..),

    -- * Compulsory lowering
    lowerPseudoOps,
    lowerNewtypes,
    lowerNewtypesWithInterface,
    NewtypeInterface,
    extractNewtypeInterface,

    -- * Optional optimization
    eliminateDeadCode,
    optimizeProgram,
    maximumProgramUnique,

    -- * Executable entry point
    MainEntrypointError (..),
    addMainEntrypoint,
    mainEntryBindingName,

    -- * Module merge
    FcMergeError (..),
    mergePrograms,

    -- * Analysis and interfaces
    ReachabilityInterface,
    extractReachabilityInterface,
    reachablePrimitiveNames,
    AxiomInterface,
    extractAxiomInterface,
    lookupAxiomDecl,

    -- * Lint
    lintProgram,
    lintProgramWithAxiomInterface,
    lintExpr,
    LintError (..),
    LintEnv (..),
    emptyLintEnv,

    -- * Desugaring
    desugarModuleWithInterface,
    DesugarConfig (..),
    DesugarResult (..),
  )
where

import Aihc.Fc.Axiom (AxiomInterface, extractAxiomInterface, lookupAxiomDecl)
import Aihc.Fc.DeadCode (ReachabilityInterface, eliminateDeadCode, extractReachabilityInterface, reachablePrimitiveNames)
import Aihc.Fc.Desugar (DesugarConfig (..), DesugarResult (..), desugarModuleWithInterface)
import Aihc.Fc.Eval (EvalError (..), Value (..), evalExpr, evalProgramBinding, renderRawValue, renderValue)
import Aihc.Fc.Lint (LintEnv (..), LintError (..), emptyLintEnv, lintExpr, lintProgram, lintProgramWithAxiomInterface)
import Aihc.Fc.Lower (lowerPseudoOps)
import Aihc.Fc.Main (MainEntrypointError (..), addMainEntrypoint, mainEntryBindingName)
import Aihc.Fc.Merge (FcMergeError (..), mergePrograms)
import Aihc.Fc.Newtype (NewtypeInterface, extractNewtypeInterface, lowerNewtypes, lowerNewtypesWithInterface)
import Aihc.Fc.Optimize (optimizeProgram)
import Aihc.Fc.Parser (FcParseError, parseExpr, parseProgram, parseType, renderParseError)
import Aihc.Fc.Pretty (renderExpr, renderProgram, renderTopBind, renderType)
import Aihc.Fc.Subst (maximumProgramUnique)
import Aihc.Fc.Syntax
