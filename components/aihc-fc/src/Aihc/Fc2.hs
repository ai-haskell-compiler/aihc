-- | System FC 2 language.
module Aihc.Fc2
  ( module Aihc.Fc2.Syntax,
    module Aihc.Fc2.Name,
    renderProgram,
    renderType,
    renderExpr,
    parseProgram,
    renderParseError,
    Fc2ParseError,
    desugarModuleFc2,
    DesugarConfig (..),
    Fc2DesugarResult (..),
    lintPrograms,
    loadScopeClosure,
    ModuleLoader,
    storeModuleLoader,
    LintError (..),
  )
where

import Aihc.Fc2.Desugar (DesugarConfig (..), Fc2DesugarResult (..), desugarModuleFc2)
import Aihc.Fc2.Lint (LintError (..), ModuleLoader, lintPrograms, loadScopeClosure, storeModuleLoader)
import Aihc.Fc2.Name
import Aihc.Fc2.Parser (Fc2ParseError, parseProgram, renderParseError)
import Aihc.Fc2.Pretty (renderExpr, renderProgram, renderType)
import Aihc.Fc2.Syntax
