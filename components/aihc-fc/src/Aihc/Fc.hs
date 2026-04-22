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

    -- * Lint
    lintProgram,
    lintExpr,
    LintError (..),
    LintEnv (..),
    emptyLintEnv,

    -- * Desugaring
    desugarModule,
    DesugarResult (..),
  )
where

import Aihc.Fc.Desugar (DesugarResult (..), desugarModule)
import Aihc.Fc.Lint (LintEnv (..), LintError (..), emptyLintEnv, lintExpr, lintProgram)
import Aihc.Fc.Pretty (renderExpr, renderProgram, renderTopBind, renderType)
import Aihc.Fc.Syntax
