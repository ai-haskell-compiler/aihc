-- | System FC core language with desugaring and lint.
--
-- This module re-exports the key types and functions for working with
-- System FC Core:
--
-- * 'Aihc.Fc.Syntax' — Core grammar (expressions, bindings, alternatives)
-- * 'Aihc.Fc.Text' — lossless, parseable Core text
-- * 'Aihc.Fc.Lint' — Structural type checker for Core
-- * 'Aihc.Fc.Desugar' — Translation from TC-annotated surface AST to Core
module Aihc.Fc
  ( -- * Syntax
    module Aihc.Fc.Syntax,

    -- * Lossless text
    renderProgram,
    parseProgram,

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
    desugarModule,
    desugarModuleWithBindings,
    desugarModuleWithTcResult,
    DesugarResult (..),
  )
where

import Aihc.Fc.Axiom (AxiomInterface, extractAxiomInterface, lookupAxiomDecl)
import Aihc.Fc.DeadCode (ReachabilityInterface, eliminateDeadCode, extractReachabilityInterface, reachablePrimitiveNames)
import Aihc.Fc.Desugar (DesugarResult (..), desugarModule, desugarModuleWithBindings, desugarModuleWithTcResult)
import Aihc.Fc.Eval (EvalError (..), Value (..), evalExpr, evalProgramBinding, renderRawValue, renderValue)
import Aihc.Fc.Lint (LintEnv (..), LintError (..), emptyLintEnv, lintExpr, lintProgram, lintProgramWithAxiomInterface)
import Aihc.Fc.Lower (lowerPseudoOps)
import Aihc.Fc.Newtype (NewtypeInterface, extractNewtypeInterface, lowerNewtypes, lowerNewtypesWithInterface)
import Aihc.Fc.Optimize (optimizeProgram)
import Aihc.Fc.Syntax
import Aihc.Fc.Text (parseProgram, renderProgram)
