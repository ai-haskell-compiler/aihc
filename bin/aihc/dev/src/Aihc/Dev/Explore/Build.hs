-- | The builds of the explorer. Each optimization level is one real build
-- into its own temporary store. The compile observer of the build gives the
-- source of each module and the output of each phase, and the explorer keeps
-- only the rendered text.
module Aihc.Dev.Explore.Build
  ( BuildResult (..),
    ModuleSource (..),
    UnitOutput,
    BuildEvent (..),
    runExplorerBuild,
  )
where

import Aihc.Cli.Build (buildWithConfig)
import Aihc.Cli.Install (CompileObservation (..), ModuleCompileConfig (..), PhaseOutput (..))
import Aihc.Cli.Options (BuildOptions (..), defaultPlanOptions)
import Aihc.Cli.Progress (ProgressEvent (..), ProgressItem (..), ProgressReporter (..))
import Aihc.Dev.Explore.Backend (backendDocuments)
import Aihc.Dev.Explore.Document (Definition (..), DefinitionKind (..), Document (..), Section (..), Segment (..), Stage (..), documentFromSections)
import Aihc.Dev.Explore.TextMate (Grammar, Token (..), fcGrammar, grinGrammar, tokenizeLines)
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Native (NativeTarget, OptimizationLevel, renderOptimizationLevel)
import Aihc.Resolve (Package (..), packageIdText)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, evaluate, try)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath ((</>))

-- | The source file of a module, and the package it belongs to.
data ModuleSource = ModuleSource
  { sourcePackage :: !Text,
    sourcePath :: !FilePath
  }

-- | The document of each phase for one unit: a module, or @program@, the
-- merged program of a whole-program build.
type UnitOutput = Map Stage Document

data BuildResult = BuildResult
  { -- | The source of each module, by module name.
    resultSources :: !(Map Text ModuleSource),
    -- | The output of each unit, by unit name.
    resultUnits :: !(Map Text UnitOutput)
  }

-- | What a build tells the explorer while it runs.
data BuildEvent
  = BuildProgress !OptimizationLevel !Text
  | BuildFinished !OptimizationLevel !(Either Text BuildResult)

-- | Build the input at one level under a directory of its own. The events
-- go to the callback, from the threads of the build.
runExplorerBuild :: FilePath -> NativeTarget -> FilePath -> OptimizationLevel -> (BuildEvent -> IO ()) -> IO ()
runExplorerBuild input target directory level send = do
  sources <- newIORef Map.empty
  units <- newIORef Map.empty
  progress <- newMVar (ProgressState Set.empty Set.empty Nothing 0)
  let observe observation =
        case observation of
          ObservedSource package name path ->
            atomicModifyIORef' sources (\known -> (Map.insert name (ModuleSource (packageText package) path) known, ()))
          ObservedPhases output -> do
            documents <- evaluate (phaseDocuments target output)
            mapM_ forceDocument documents
            atomicModifyIORef' units (\known -> (Map.insert (phaseModule output) documents known, ()))
      reporter =
        ProgressReporter
          { progressReport = \event -> do
              modifyMVar_ progress (pure . advance event)
              state <- readMVar progress
              send (BuildProgress level (renderProgress state)),
            progressColor = False
          }
      options =
        BuildOptions
          { buildInput = input,
            buildSourceDirectories = ["."],
            buildPackageConstraints = [],
            buildTarget = target,
            buildStoreRoot = Just (directory </> "store"),
            buildBuildRoot = Just (directory </> "build"),
            buildWorkspace = Nothing,
            buildKeepCore = False,
            buildKeepGrin = False,
            buildKeepLir = False,
            buildKeepNative = False,
            buildLint = False,
            buildCheckPrimBounds = False,
            buildProfileAllocations = False,
            buildLto = False,
            buildOptimization = level,
            buildNoLink = False,
            buildVerbose = False,
            buildOutput = Just (directory </> "bin"),
            buildExecutables = [],
            buildPlanOptions = defaultPlanOptions
          }
  send (BuildProgress level ("Build " <> T.pack (renderOptimizationLevel level)))
  outcome <- try (buildWithConfig reporter options (\config -> config {compileObserver = Just observe}))
  case outcome of
    Left failure -> send (BuildFinished level (Left (T.pack (show (failure :: SomeException)))))
    Right _ -> do
      result <- BuildResult <$> readIORef sources <*> readIORef units
      send (BuildFinished level (Right result))
  where
    packageText (Package name identifier) = name <> " (" <> packageIdText identifier <> ")"

-- | What the progress line counts.
data ProgressState = ProgressState
  { progressItems :: !(Set.Set ProgressItem),
    progressDone :: !(Set.Set ProgressItem),
    progressCurrent :: !(Maybe ProgressItem),
    progressModules :: !Int
  }

advance :: ProgressEvent -> ProgressState -> ProgressState
advance event state =
  case event of
    ProgressPlan items -> state {progressItems = Set.fromList items}
    ProgressCompile item -> state {progressCurrent = Just item}
    ProgressBuild item _ -> state {progressCurrent = Just item}
    ProgressLink item -> state {progressCurrent = Just item}
    ProgressModules _ count -> state {progressModules = progressModules state + count}
    ProgressDone item -> state {progressDone = Set.insert item (progressDone state)}
    ProgressStore item -> state {progressDone = Set.insert item (progressDone state)}
    _ -> state

renderProgress :: ProgressState -> Text
renderProgress state =
  T.pack (show (Set.size (progressDone state)))
    <> "/"
    <> T.pack (show (Set.size (progressItems state)))
    <> " packages, "
    <> T.pack (show (progressModules state))
    <> " modules"
    <> maybe "" (\item -> ", now " <> itemText item) (progressCurrent state)
  where
    itemText item = case item of
      ItemPackage name -> name
      ItemExecutable name -> name <> " (executable)"

-- | Render the phases of a unit. Each program is split at its top-level
-- definitions.
phaseDocuments :: NativeTarget -> PhaseOutput -> UnitOutput
phaseDocuments target output =
  Map.fromList
    [ (StageFc, documentFromSections (highlightWith fcGrammar) (fcSections (phaseFc output))),
      (StageGrin, documentFromSections (highlightWith grinGrammar) (grinSections (phaseGrin output))),
      (StageCpsGrin, documentFromSections (highlightWith grinGrammar) (grinSections (phaseCpsGrin output))),
      (StageGcGrin, documentFromSections (highlightWith grinGrammar) (grinSections (Grin.gcGrinProgram (phaseGcGrin output))))
    ]
    <> backendDocuments target (phaseLirSettings output) (phaseGcGrin output)

fcSections :: Fc.Program -> [Section]
fcSections program =
  [ Section (fmap definitionOf name) (T.lines text)
  | (name, text) <- Fc.renderProgramSections program
  ]
  where
    definitionOf name =
      case Fc.nameOrigin name of
        Fc.OriginTop _ moduleName -> Definition (Just moduleName) (Fc.nameText name) DefinitionCode
        Fc.OriginLocal _ -> Definition Nothing (Fc.nameText name) DefinitionCode

grinSections :: Grin.GrinProgram -> [Section]
grinSections program =
  [ Section (fmap definitionOf name) (T.lines (renderStrict (layoutPretty defaultLayoutOptions document)))
  | (name, document) <- Grin.prettyProgramSections program
  ]
  where
    -- A GRIN function name starts with @$@. A global or a constructor is
    -- data.
    definitionOf name =
      case Grin.grinNameScope name of
        Just (scope, baseName) -> Definition (Just (Grin.grinScopeModule scope)) baseName (kindOf baseName)
        Nothing -> Definition Nothing name (kindOf name)
    kindOf name = if "$" `T.isPrefixOf` name then DefinitionCode else DefinitionData

-- | The segments of each line. The class of a token is its innermost scope.
highlightWith :: Grammar -> [Text] -> [[Segment]]
highlightWith grammar textLines =
  [[Segment (tokenText token) (innermost (tokenScopes token)) | token <- tokens] | tokens <- tokenizeLines grammar textLines]
  where
    innermost scopes = if null scopes then "" else last scopes

-- | Force the text and the definitions of a document, so that the program
-- values of the build can be collected. The segments stay lazy.
forceDocument :: Document -> IO ()
forceDocument document = do
  evaluate (rnf (documentLines document))
  evaluate (rnf [(line, definitionModule definition, definitionName definition) | (line, definition) <- documentDefinitions document])
