-- | System FC language.
module Aihc.Fc
  ( module Aihc.Fc.Syntax,
    module Aihc.Fc.Name,
    renderProgram,
    renderProgramSections,
    encodeProgram,
    decodeProgram,
    readProgramFile,
    writeProgramFile,
    programFileName,
    parseProgram,
    renderParseError,
    FcParseError,
    tidyProgram,
    shareProgram,
    mergePrograms,
    pruneProgram,
    inlineProgram,
    etaExpandProgram,
    demandProgram,
    simplifyProgram,
    EtaReport (..),
    DemandReport (..),
    DemandRewrites (..),
    InlineConfig (..),
    InlinePolicy (..),
    shrinkPolicy,
    growPolicy,
    InlineReport (..),
    SimplifyReport (..),
    Pass (..),
    PassReport (..),
    SplitScope (..),
    passName,
    runPass,
    runPasses,
    programSize,
    desugarModuleFc,
    DesugarConfig (..),
    exportListDesugarConfig,
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
import Aihc.Fc.Binary (decodeProgram, encodeProgram, programFileName, readProgramFile, writeProgramFile)
import Aihc.Fc.Demand (DemandReport (..), DemandRewrites (..), demandProgram)
import Aihc.Fc.Desugar (DesugarConfig (..), FcDesugarResult (..), allPublicDesugarConfig, desugarModuleFc, exportListDesugarConfig)
import Aihc.Fc.Inline (InlineConfig (..), InlinePolicy (..), InlineReport (..), growPolicy, inlineProgram, shrinkPolicy)
import Aihc.Fc.Lint (LintError (..), ModuleLoader, lintProgram, loadScopeClosure, storeModuleLoader)
import Aihc.Fc.Merge (mergePrograms)
import Aihc.Fc.Name
import Aihc.Fc.Parser (FcParseError, parseProgram, renderParseError)
import Aihc.Fc.Pass (Pass (..), PassReport (..), passName, runPass, runPasses)
import Aihc.Fc.Pretty (renderProgram, renderProgramSections)
import Aihc.Fc.Prune (pruneProgram)
import Aihc.Fc.Share (shareProgram)
import Aihc.Fc.Simplify (SimplifyReport (..), simplifyProgram)
import Aihc.Fc.Size (programSize)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.WorkerWrapper (SplitScope (..))
