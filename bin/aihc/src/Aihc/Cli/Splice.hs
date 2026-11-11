{-# LANGUAGE OverloadedStrings #-}

-- | Run the splices of a module in a WebAssembly engine.
--
-- A build that has 'withSplices' in its compile config gives each module
-- with splices to 'runSplices'. For one module, 'runSplices' does these
-- steps:
--
-- 1. Build an engine image for @wasm32-wasip3@. The image has the guest
--    program and the packages that the module can import.
-- 2. Write a package with one module, the splice module of
--    "Aihc.Th.Splices", and install it with the position-independent code
--    model.
-- 3. Link the package as a shared module and load it into the image.
-- 4. Give the caller a function that runs the closure of one splice,
--    answers its requests, and converts its result to the surface syntax.
--    The caller runs the splices in source order.
--
-- The splice code has no access to the build machine. See
-- "Aihc.Wasm.Engine".
--
-- The body of a splice cannot use a module of its own package. For
-- @reify@, the build gives the facts of the names of the module and of
-- its own package. The facts of the other names come from the type
-- artifacts of the packages that the splice package depends on, in the
-- store.
module Aihc.Cli.Splice
  ( withSplices,
    runSplices,
  )
where

import Aihc.Cli.Build (buildWithConfig)
import Aihc.Cli.Install (InstallResult (..), ModuleCompileConfig (..), installWithConfig)
import Aihc.Cli.ModuleProvider (moduleNameDirectory, readTypeArtifactFile)
import Aihc.Cli.Options (BuildOptions (..), InstallOptions (..), defaultPlanOptions)
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest)
import Aihc.Cli.Progress (quietProgress)
import Aihc.Cli.TypeArtifact (TypeArtifact (..))
import Aihc.Native (NativeTarget (..), OptimizationLevel (..), nativeTargetStoreDirectory)
import Aihc.Resolve.Types (GlobalName (..), PackageId (..))
import Aihc.Th.Convert (declarationsFromTree, expressionFromTree)
import Aihc.Th.Reify (reifyInfo)
import Aihc.Th.Request (Request (..), reportedFailure, requestFromTree)
import Aihc.Th.Splices (RunSplice, Splice (..), SpliceRequest (..), SpliceResult (..), spliceClosureName, spliceModuleName, spliceModuleText)
import Aihc.Th.Wire (Tree (..), decodeTree, encodeTree)
import Aihc.Wasm.Engine (engineGuestPath, linkSharedModule, loadSharedModule, runClosure, startGuest, withEngine)
import Control.Exception (IOException, catch, try)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, toLower)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr, stdout)
import System.IO.Error (ioeGetErrorString)

-- | Make a build run the expression splices of its modules. The builds
-- that 'runSplices' starts do the same, so a package of the image can also
-- have splices.
withSplices :: ModuleCompileConfig -> ModuleCompileConfig
withSplices config = config {compileSplices = Just runSplices}

-- | Prepare the splices of a request, and give a function that runs one
-- splice to the action. The action runs the splices in the order of the
-- request.
runSplices :: SpliceRequest -> (RunSplice -> IO a) -> IO a
runSplices request action =
  run `catch` \failure ->
    ioError (userError ("Template Haskell failed in module " <> T.unpack (spliceRequestModule request) <> ": " <> ioeGetErrorString (failure :: IOException)))
  where
    root = spliceRequestBuildRoot request
    packageDirectory = root </> "package"
    package = splicePackageName request
    run = do
      image <- buildImage request
      createDirectoryIfMissing True (packageDirectory </> "src")
      writeFile (packageDirectory </> T.unpack package <> ".cabal") (splicePackageDescription request)
      TIO.writeFile (packageDirectory </> "src" </> T.unpack spliceModuleName <> ".hs") (spliceModuleText request)
      installed <- installWithConfig (quietProgress stdout) (packageOptions request packageDirectory) withSplices
      let shared = root </> "splices.wasm"
      linkSharedModule shared [] [installStorePath installed </> "lib" </> "lib" <> T.unpack package <> ".a"]
      packages <- packageDirectories (spliceRequestStoreRoot request </> nativeTargetStoreDirectory Wasm32Wasip3) (installStorePath installed)
      storeFacts <- newReifier packages
      withEngine image $ \engine -> do
        startGuest engine
        loadSharedModule engine shared
        action $ \index localFacts -> do
          splice <- maybe (ioError (userError ("The request has no splice " <> show index <> "."))) pure (lookup index (zip [0 ..] (spliceRequestSplices request)))
          let reify global = localFacts global >>= maybe (storeFacts global) pure
          requests <- newIORef []
          outcome <- try (runClosure engine (answerRequest reify requests) (package <> "-" <> splicePackageVersion) spliceModuleName (spliceClosureName index))
          made <- reverse <$> readIORef requests
          forM_ [message | RequestReport False message <- made] $ \message ->
            hPutStrLn stderr ("warning: Template Haskell in module " <> T.unpack (spliceRequestModule request) <> ": " <> T.unpack message)
          let convert tree =
                case splice of
                  ExpressionSplice _ -> ExpressionResult <$> expressionFromTree tree
                  DeclarationSplice _ -> DeclarationResult <$> declarationsFromTree tree
          -- A report of an error is the cause of a failure of the closure,
          -- so the report comes before the failure itself.
          case (reportedFailure made, outcome) of
            (Just message, _) -> ioError (userError (T.unpack message))
            (Nothing, Left failure) -> ioError (failure :: IOException)
            (Nothing, Right [message]) -> either (ioError . userError . T.unpack) pure (decodeTree message >>= convert)
            (Nothing, Right messages) -> ioError (userError ("a splice sent " <> show (length messages) <> " messages"))

-- | Do the request of a splice and give the bytes of the reply. Each
-- request goes to the list. The reply to a report has no bytes. The reply
-- to @reify@ is an @Either String Info@ value.
answerRequest :: (GlobalName -> IO (Either Text Tree)) -> IORef [Request] -> ByteString -> IO ByteString
answerRequest reify requests bytes =
  case decodeTree bytes >>= requestFromTree of
    Right made -> do
      modifyIORef' requests (made :)
      case made of
        RequestReport {} -> pure BS.empty
        RequestReify name global -> do
          answer <- maybe (pure (Left ("The compiler can only give the facts of a name with a package and a module, and " <> name <> " does not have them."))) reify global
          pure . encodeTree $ case answer of
            Right info -> TreeNode "Right" [info]
            Left message -> TreeNode "Left" [TreeString message]
    Left message -> ioError (userError (T.unpack message))

-- | The store directory of each package below a package, by the package
-- id that names of the package have. The manifest of a package names the
-- store directories of the packages that it depends on. A package that is
-- not in the store, such as a local package, is not in the result.
packageDirectories :: FilePath -> FilePath -> IO (Map PackageId FilePath)
packageDirectories storeRoot root = do
  manifest <- readManifest root
  go Map.empty (maybe [] packageManifestDependencies manifest)
  where
    go found [] = pure found
    go found (identity : rest) = do
      let directory = storeRoot </> T.unpack identity
      manifest <- readManifest directory
      case manifest of
        Just dependency
          | not (Map.member (PackageId (packageManifestUnitId dependency)) found) ->
              go (Map.insert (PackageId (packageManifestUnitId dependency)) directory found) (packageManifestDependencies dependency <> rest)
        _ -> go found rest
    readManifest directory = do
      let path = packageManifestPath directory
      exists <- doesFileExist path
      if exists
        then readPackageManifest path >>= either (\message -> ioError (userError ("Invalid package manifest " <> path <> ": " <> message))) (pure . Just)
        else pure Nothing

-- | Give the facts of a name from the type artifact of its module. The
-- reifier reads each artifact one time.
newReifier :: Map PackageId FilePath -> IO (GlobalName -> IO (Either Text Tree))
newReifier packages = do
  artifacts <- newIORef Map.empty
  let artifactOf directory home = do
        cached <- Map.lookup (directory, home) <$> readIORef artifacts
        case cached of
          Just artifact -> pure artifact
          Nothing -> do
            let path = directory </> moduleNameDirectory home </> "type.cbor"
            exists <- doesFileExist path
            artifact <- if exists then Just <$> readTypeArtifactFile path else pure Nothing
            atomicModifyIORef' artifacts (\known -> (Map.insert (directory, home) artifact known, ()))
            pure artifact
  pure $ \global ->
    case Map.lookup (globalNamePackage global) packages of
      Nothing -> pure (Left ("The splice cannot use the package " <> packageIdText (globalNamePackage global) <> " of " <> globalNameText global <> "."))
      Just directory -> do
        artifact <- artifactOf directory (globalNameModule global)
        pure $ case artifact of
          Nothing -> Left ("The package " <> packageIdText (globalNamePackage global) <> " has no type facts of the module " <> globalNameModule global <> ".")
          Just found -> reifyInfo (typeArtifactInterface found) global

-- | Build the image of a request and give its path.
buildImage :: SpliceRequest -> IO FilePath
buildImage request = do
  guest <- engineGuestPath
  images <-
    buildWithConfig
      (quietProgress stdout)
      BuildOptions
        { buildInput = guest,
          buildSourceDirectories = [takeDirectory guest],
          buildPackageConstraints = map constraint (spliceRequestDependencies request) <> ["aihc-template-haskell"],
          buildTarget = Wasm32Wasip3,
          buildStoreRoot = Just (spliceRequestStoreRoot request),
          buildBuildRoot = Just (spliceRequestBuildRoot request </> "image"),
          buildWorkspace = Nothing,
          buildKeepCore = False,
          buildKeepGrin = False,
          buildKeepLir = False,
          buildKeepNative = False,
          buildLint = False,
          buildCheckPrimBounds = False,
          buildProfileAllocations = False,
          buildProfileTime = False,
          buildLto = False,
          buildOptimization = O0,
          buildNoLink = False,
          buildOnlyDependencies = False,
          buildNoPie = False,
          buildWasmImage = True,
          buildVerbose = False,
          buildPrintTimings = False,
          buildOutput = Just (spliceRequestBuildRoot request </> "image.wasm"),
          buildExecutables = [],
          buildPlanOptions = defaultPlanOptions
        }
      withSplices
  case images of
    [image] -> pure image
    _ -> ioError (userError "the image build gave no image")
  where
    constraint (name, version) = T.unpack name <> " == " <> renderVersion version

-- | The options that install the splice package for a shared module.
packageOptions :: SpliceRequest -> FilePath -> InstallOptions
packageOptions request directory =
  InstallOptions
    { installPackageTarget = directory,
      installStoreRoot = Just (spliceRequestStoreRoot request),
      installBuildRoot = Just (spliceRequestBuildRoot request </> "package-build"),
      installImmutable = False,
      installKeepCore = False,
      installKeepGrin = False,
      installKeepNative = False,
      installLint = False,
      installCheckPrimBounds = False,
      installLto = False,
      installOptimization = O0,
      installReinstall = False,
      installNoCode = False,
      installPositionIndependent = True,
      installVerbose = False,
      installPrintTimings = False,
      installTarget = Wasm32Wasip3,
      installWorkspace = Nothing,
      installPlanOptions = defaultPlanOptions
    }

-- | The name of the splice package. Each module has its own package, so
-- the symbols of two splice modules in one image are different.
splicePackageName :: SpliceRequest -> Text
splicePackageName request =
  T.intercalate "-" ("aihc-splice" : map part [spliceRequestPackage request, spliceRequestModule request])
  where
    -- A package name has letters, digits, and hyphens.
    part = T.map (\char -> if isAlphaNum char then toLower char else '-')

splicePackageVersion :: Text
splicePackageVersion = "0"

-- | The Cabal file of the splice package. The package depends on each
-- package that the module can import, at the version that the build of the
-- module uses.
splicePackageDescription :: SpliceRequest -> String
splicePackageDescription request =
  unlines
    ( [ "cabal-version: 3.0",
        "name: " <> T.unpack (splicePackageName request),
        "version: " <> T.unpack splicePackageVersion,
        "build-type: Simple",
        "",
        "library",
        "  exposed-modules: " <> T.unpack spliceModuleName,
        "  hs-source-dirs: src",
        "  build-depends:"
      ]
        <> ["    " <> T.unpack name <> " ==" <> renderVersion version <> "," | (name, version) <- spliceRequestDependencies request]
        <> ["    template-haskell," | "template-haskell" `notElem` map fst (spliceRequestDependencies request)]
        <> ["  default-extensions:" | not (null extensions)]
        <> ["    " <> extension | extension <- extensions]
        <> ["  default-language: " <> language | Just language <- [spliceRequestLanguage request]]
    )
  where
    extensions = spliceRequestExtensions request

renderVersion :: [Int] -> String
renderVersion = intercalate "." . map show
