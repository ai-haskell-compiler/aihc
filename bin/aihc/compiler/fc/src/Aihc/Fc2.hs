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
    tidyProgram,
    desugarModuleFc2,
    typeEnvFromTcInterface,
    valueDesugarSupportTerms,
    DesugarConfig (..),
    Fc2DesugarResult (..),
    lintProgram,
    loadScopeClosure,
    ModuleLoader,
    storeModuleLoader,
    LintError (..),
  )
where

import Aihc.Fc2.Desugar (DesugarConfig (..), Fc2DesugarResult (..), desugarModuleFc2, typeEnvFromTcInterface, valueDesugarSupportTerms)
import Aihc.Fc2.Lint (LintError (..), ModuleLoader, lintProgram, loadScopeClosure, storeModuleLoader)
import Aihc.Fc2.Name
import Aihc.Fc2.Parser (Fc2ParseError, parseProgram, renderParseError)
import Aihc.Fc2.Pretty (renderExpr, renderProgram, renderType)
import Aihc.Fc2.Syntax
import Aihc.Fc2.Tidy (tidyProgram)
