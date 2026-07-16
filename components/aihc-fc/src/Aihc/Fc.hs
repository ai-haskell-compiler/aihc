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

    -- * Evaluation
    evalProgramBinding,
    evalExpr,
    renderValue,
    renderRawValue,
    EvalError (..),
    Value (..),

    -- * Optimization
    eliminateDeadCode,
    lowerNewtypes,

    -- * Lint
    lintProgram,
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

import Aihc.Fc.DeadCode (eliminateDeadCode)
import Aihc.Fc.Desugar (DesugarResult (..), desugarModule, desugarModuleWithBindings, desugarModuleWithTcResult)
import Aihc.Fc.Eval (EvalError (..), Value (..), evalExpr, evalProgramBinding, renderRawValue, renderValue)
import Aihc.Fc.Lint (LintEnv (..), LintError (..), emptyLintEnv, lintExpr, lintProgram)
import Aihc.Fc.Newtype (lowerNewtypes)
import Aihc.Fc.Pretty (renderExpr, renderProgram, renderTopBind, renderType)
import Aihc.Fc.Syntax
