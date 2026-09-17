-- | System FC language.
module Aihc.Fc
  ( module Aihc.Fc.Syntax,
    module Aihc.Fc.Name,
    renderProgram,
    parseProgram,
    renderParseError,
    FcParseError,
    tidyProgram,
    shareProgram,
    mergePrograms,
    pruneProgram,
    inlineProgram,
    etaExpandProgram,
    simplifyProgram,
    EtaReport (..),
    InlineConfig (..),
    InlinePolicy (..),
    shrinkPolicy,
    growPolicy,
    InlineReport (..),
    SimplifyReport (..),
    Pass (..),
    PassReport (..),
    passName,
    runPass,
    runPasses,
    programSize,
    desugarModuleFc,
    DesugarConfig (..),
    moduleDesugarConfig,
    allPublicDesugarConfig,
    FcDesugarResult (..),
    lintProgram,
    loadScopeClosure,
    ModuleLoader,
    storeModuleLoader,
    LintError (..),
  )
where

import Aihc.Fc.Arity (EtaReport (..), etaExpandProgram)
import Aihc.Fc.Desugar (DesugarConfig (..), FcDesugarResult (..), allPublicDesugarConfig, desugarModuleFc, moduleDesugarConfig)
import Aihc.Fc.Inline (InlineConfig (..), InlinePolicy (..), InlineReport (..), growPolicy, inlineProgram, shrinkPolicy)
import Aihc.Fc.Lint (LintError (..), ModuleLoader, lintProgram, loadScopeClosure, storeModuleLoader)
import Aihc.Fc.Merge (mergePrograms)
import Aihc.Fc.Name
import Aihc.Fc.Parser (FcParseError, parseProgram, renderParseError)
import Aihc.Fc.Pass (Pass (..), PassReport (..), passName, runPass, runPasses)
import Aihc.Fc.Pretty (renderProgram)
import Aihc.Fc.Prune (pruneProgram)
import Aihc.Fc.Share (shareProgram)
import Aihc.Fc.Simplify (SimplifyReport (..), simplifyProgram)
import Aihc.Fc.Size (programSize)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
