{-# LANGUAGE OverloadedStrings #-}

-- | Run the Template Haskell splices of the module of the reader.
--
-- The helper makes the splice module of "Aihc.Th.Splices", and compiles
-- it to WebAssembly assembly with the position-independent code model.
-- The host assembles and links that assembly as a shared module, and
-- loads it into an engine of "Aihc.Wasm.Engine". In the browser, the
-- engine is the image of the web kit, and the worker of the kit is the
-- host. The helper then runs the splices one at a time, in source order,
-- and answers their requests from the facts of the session.
--
-- The splice module can only use the modules of the bundle. The module of
-- the reader is the only module of its program, so a splice cannot use a
-- different module of the program, as with GHC.
module Aihc.Play.Splices
  ( SpliceHost (..),
    noSpliceHost,
    expandUserSplices,
  )
where

import Aihc.Parser.Syntax (Decl, Extension (..), Module (..), ModuleHead (..))
import Aihc.Play.Compile (compileCheckedModule)
import Aihc.Play.Session
import Aihc.Prim.Wiring (builtinFunctionModules)
import Aihc.Resolve
  ( ModuleKey (..),
    ModuleUnit (..),
    Package (..),
    PackageId (..),
    builtins,
    collectModuleExportsWithDeps,
    lookupModuleExport,
    moduleExportKeys,
    moduleTopLevelName,
  )
import Aihc.Resolve.Types (GlobalName (..))
import Aihc.Tc (TcInterface)
import Aihc.Th.Prepare (PreparedSplices (..), prepareSplices)
import Aihc.Th.Reify (exportedFixity)
import Aihc.Th.Run (SpliceEnvironment (..), describeFailure, runSpliceClosure)
import Aihc.Th.Splices (SpliceRequest (..), expandSplices, moduleSplices, spliceClosureName, spliceModuleName, spliceModuleText, splicePackageId, splicePackageName)
import Aihc.Wasm.Engine.Protocol (Engine, runClosure, startGuest)
import Aihc.Wasm.Lir (CodeModel (..))
import Control.Exception (IOException, bracket, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.IO (hPutStrLn, stderr)

-- | What the helper needs from its host to run splices.
data SpliceHost = SpliceHost
  { -- | Start an engine with an instance of the image.
    hostOpenEngine :: IO Engine,
    -- | Assemble and link the assembly of a module as a shared module, and
    -- load it into the engine.
    hostLoadAssembly :: Engine -> Text -> IO (),
    -- | Stop the engine.
    hostCloseEngine :: Engine -> IO ()
  }

-- | A host that cannot run splices.
noSpliceHost :: SpliceHost
noSpliceHost =
  SpliceHost
    { hostOpenEngine = ioError (userError "This helper cannot run Template Haskell splices."),
      hostLoadAssembly = \_ _ -> pure (),
      hostCloseEngine = \_ -> pure ()
    }

-- | The module with the results of its splices in the positions of the
-- splices. A module without the extension @TemplateHaskell@ or
-- @QuasiQuotes@, or without a splice, does not change.
expandUserSplices :: Session -> SpliceHost -> Package -> Module -> [Extension] -> IO (Either [Problem] Module)
expandUserSplices session host package modu extensions
  | not (any (`elem` extensions) [TemplateHaskell, QuasiQuotes]) = pure (Right modu)
  | null (moduleSplices modu) = pure (Right modu)
  | otherwise =
      case compileSpliceModule session request of
        Left problems -> pure (Left problems)
        Right assembly -> do
          outcome <- try (bracket (hostOpenEngine host) (hostCloseEngine host) (run assembly))
          pure $ case outcome of
            Right expanded -> Right expanded
            Left failure -> Left [Problem "splice" Nothing (T.pack (describeFailure (failure :: IOException)))]
  where
    unit = ModuleUnit package extensions modu
    visible = collectModuleExportsWithDeps (sessionExports session) [unit] <> sessionExports session
    scope = builtins package visible builtinFunctionModules
    ownModule = maybe "Main" (\header -> moduleHeadName header) (moduleHead modu)
    template = spliceRequestFor package ownModule modu extensions
    prepared = prepareSplices scope visible unit (packageId package) (splicePackageId template) (moduleImports modu)
    request =
      template
        { spliceRequestSplices = preparedSplices prepared,
          spliceRequestSpans = preparedSpans prepared,
          spliceRequestImports = preparedImports prepared
        }
    bound = preparedModule prepared
    isOwn global = globalNamePackage global == packageId package && globalNameModule global == ownModule
    run assembly engine = do
      startGuest engine
      hostLoadAssembly host engine assembly
      prefixFacts <- newIORef Map.empty
      let environment =
            SpliceEnvironment
              { spliceEnvironmentRequest = request,
                spliceEnvironmentPackageIds = (splicePackageId request, spliceRequestPackageId request),
                spliceEnvironmentPrimPackage = Just (sessionPrim session),
                spliceEnvironmentFacts = \answer global -> pure (answer (sessionInterface session) global),
                spliceEnvironmentFixity = pure . Right . importedFixity,
                spliceEnvironmentWarn = \message -> hPutStrLn stderr ("warning: Template Haskell: " <> T.unpack message)
              }
          -- The facts of a name of this module come from the declarations
          -- above the splice. The helper checks them one time for each
          -- prefix.
          facts prefix answer global
            | isOwn global = do
                known <- Map.lookup (length prefix) <$> readIORef prefixFacts
                checked <- case known of
                  Just result -> pure result
                  Nothing -> do
                    let result = checkPrefix session package bound extensions prefix
                    modifyIORef' prefixFacts (Map.insert (length prefix) result)
                    pure result
                pure (Just (checked >>= \interface -> answer interface global))
            | otherwise = pure Nothing
          fixities prefix global
            | isOwn global =
                let prefixExports = collectModuleExportsWithDeps visible [prefixUnit package bound extensions prefix]
                 in pure (Just (lookupModuleExport (ModuleKey package ownModule) prefixExports >>= (`exportedFixity` global)))
            | otherwise = pure Nothing
          runAt index prefix current =
            snd
              <$> runSpliceClosure
                (\answer -> runClosure engine answer (splicePackageId request) spliceModuleName (spliceClosureName index))
                environment
                index
                (moduleTopLevelName scope visible unit {moduleUnitAst = current})
                (facts prefix)
                (fixities prefix)
      expandSplices (length (preparedSplices prepared)) runAt bound
    -- The fixity of a name of a module of the bundle.
    importedFixity global =
      case [key | key@(ModuleKey owner name) <- moduleExportKeys visible, packageId owner == globalNamePackage global, name == globalNameModule global] of
        key : _ -> lookupModuleExport key visible >>= (`exportedFixity` global)
        [] -> Nothing

-- | The request of the module of the reader. The helper has no files, no
-- store, and no other module of the package.
spliceRequestFor :: Package -> Text -> Module -> [Extension] -> SpliceRequest
spliceRequestFor package ownModule modu extensions =
  SpliceRequest
    { spliceRequestPackage = packageName package,
      spliceRequestPackageId = packageIdText (packageId package),
      spliceRequestModule = ownModule,
      spliceRequestStoreRoot = "",
      spliceRequestBuildRoot = "",
      spliceRequestPackageRoot = "",
      spliceRequestDependencies = [],
      spliceRequestExtensions = [],
      spliceRequestLanguage = Nothing,
      spliceRequestPragmas = moduleLanguagePragmas modu,
      spliceRequestEnabledExtensions = extensions,
      spliceRequestImports = moduleImports modu,
      spliceRequestPackageFiles = Map.empty,
      spliceRequestOwnModules = [],
      spliceRequestOwnResults = [],
      spliceRequestUnavailableImports = [],
      spliceRequestSplices = [],
      spliceRequestSourceFile = "Main.hs",
      spliceRequestSpans = [],
      spliceRequestNative = False
    }

-- | Compile the splice module of a request to assembly with the
-- position-independent code model.
compileSpliceModule :: Session -> SpliceRequest -> Either [Problem] Text
compileSpliceModule session request = do
  let source = spliceModuleText request
      (parseProblems, parsed, extensions) = parseUserModule source
      package = Package (splicePackageName request) (PackageId (splicePackageId request))
  inSplices parseProblems
  (checked, interface) <- either (Left . map inSplice) Right (checkModules session package [(withQuoteImports parsed, extensions)])
  either (Left . map inSplice) Right (mapM (compileCheckedModule PositionIndependent session interface) checked)
    >>= \assembly -> Right (T.unlines assembly)
  where
    inSplices [] = Right ()
    inSplices problems = Left (map inSplice problems)
    -- A problem of the splice module has a position in the text of that
    -- module, not in the module of the reader.
    inSplice problem = problem {problemStage = "splice", problemSpan = Nothing, problemMessage = "The splice module: " <> problemMessage problem}

-- | The declarations above a splice as a module of their own, with no
-- export list: the export list can name a declaration below.
prefixModule :: Module -> [Decl] -> Module
prefixModule modu prefix =
  modu
    { moduleHead = fmap (\header -> header {moduleHeadExports = Nothing}) (moduleHead modu),
      moduleDecls = prefix
    }

prefixUnit :: Package -> Module -> [Extension] -> [Decl] -> ModuleUnit
prefixUnit package modu extensions prefix = ModuleUnit package extensions (prefixModule modu prefix)

-- | The type facts of the declarations above a splice, with the facts of
-- the session.
checkPrefix :: Session -> Package -> Module -> [Extension] -> [Decl] -> Either Text TcInterface
checkPrefix session package modu extensions prefix =
  case checkModules session package [(prefixModule modu prefix, extensions)] of
    Left problems -> Left ("The compiler cannot check the declarations above the splice. " <> T.intercalate " " (map problemMessage problems))
    Right (_, interface) -> Right interface
