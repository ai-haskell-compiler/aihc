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
-- The body of a splice can use the modules of its own package that the
-- module imports. The splice package has a copy of each of these modules,
-- and compiles them again. The names of the copies have the package id of
-- the splice package. Thus, the compiler changes that package id to the
-- package id of the original package in each message from the splice, and
-- back in each reply. A module of the same dependency cycle as the module
-- is not available.
--
-- For @reify@, the build gives the facts of the names of the module and of
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
import Aihc.Cli.Options (BuildOptions (..), InstallOptions (..), PlanOptions (..), defaultPlanOptions)
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest)
import Aihc.Cli.Progress (quietProgress)
import Aihc.Cli.TypeArtifact (TypeArtifact (..))
import Aihc.Native (NativeTarget (..), OptimizationLevel (..), nativeTargetStoreDirectory)
import Aihc.Resolve.Types (GlobalName (..), PackageId (..))
import Aihc.Th.Convert (declarationsFromTree, expressionFromTree)
import Aihc.Th.Reify (reifyInfo)
import Aihc.Th.Request (Request (..), reportedFailure, requestFromTree)
import Aihc.Th.Splices (PackageFile (..), RunSplice, Splice (..), SpliceRequest (..), SpliceResult (..), spliceClosureName, spliceModuleName, spliceModuleText, splicePackageId, splicePackageName, splicePackageVersion)
import Aihc.Th.Wire (Tree (..), decodeTree, encodeTree)
import Aihc.Wasm.Engine (engineGuestPath, linkSharedModule, loadSharedModule, runClosure, startGuest, withEngine)
import Control.Exception (IOException, catch, try)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.FilePath (replaceExtension, takeDirectory, takeExtension, (<.>), (</>))
import System.IO (hPutStrLn, stderr, stdout)
import System.IO.Error (ioeGetErrorString, isUserError)

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
    ioError (userError ("Template Haskell failed in module " <> T.unpack (spliceRequestModule request) <> ": " <> describeFailure failure <> unavailableNote))
  where
    root = spliceRequestBuildRoot request
    -- A splice cannot use a module of the same dependency cycle. A failure
    -- names these modules, because the failure is often an unbound name of
    -- such a module.
    unavailableNote =
      case spliceRequestUnavailableImports request of
        [] -> ""
        names -> "\nThe splices of the module cannot use these modules of its own package, because they are in the same import cycle as the module: " <> intercalate ", " (map T.unpack names) <> "."
    packageDirectory = root </> "package"
    package = splicePackageName request
    packageIds = (splicePackageId request, spliceRequestPackageId request)
    run = do
      absolute <- absoluteRequest request
      image <- buildImage request
      createDirectoryIfMissing True (packageDirectory </> "src")
      writeFile (packageDirectory </> T.unpack package <> ".cabal") (splicePackageDescription absolute)
      TIO.writeFile (packageDirectory </> "src" </> T.unpack spliceModuleName <> ".hs") (spliceModuleText request)
      mapM_ (copyOwnModule absolute (packageDirectory </> "src")) (spliceRequestOwnModules absolute)
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
          outcome <- try (runClosure engine (answerRequest packageIds reify requests) (splicePackageId request) spliceModuleName (spliceClosureName index))
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
            (Nothing, Left failure) -> ioError (userError ("splice " <> show index <> describeRequests made <> ": " <> describeFailure failure))
            (Nothing, Right [message]) -> either (ioError . userError . T.unpack) pure (decodeTree message >>= convert . fromSplice packageIds)
            (Nothing, Right messages) -> ioError (userError ("a splice sent " <> show (length messages) <> " messages"))

-- | The request with absolute paths for the source files and the include
-- directories of the package. The Cabal file of the splice package names
-- them, and a relative path in that file starts at the splice package.
absoluteRequest :: SpliceRequest -> IO SpliceRequest
absoluteRequest request = do
  ownModules <- mapM (\(name, path) -> (name,) <$> makeAbsolute path) (spliceRequestOwnModules request)
  files <-
    Map.fromList
      <$> mapM
        ( \(path, file) -> do
            absolute <- makeAbsolute path
            includeDirs <- mapM makeAbsolute (packageFileIncludeDirs file)
            pure (absolute, file {packageFileIncludeDirs = includeDirs})
        )
        [entry | entry@(path, _) <- Map.toList (spliceRequestPackageFiles request), path `elem` map snd (spliceRequestOwnModules request)]
  pure request {spliceRequestOwnModules = ownModules, spliceRequestPackageFiles = files}

-- | Change the package id of the splice package to the package id of the
-- original package, in each name of a tree from the splice. The first id
-- of the pair is the id of the splice package.
fromSplice :: (Text, Text) -> Tree -> Tree
fromSplice (splice, original) = renamePackageId splice original

-- | Change the package id of the original package to the package id of the
-- splice package, in each name of a tree to the splice.
toSplice :: (Text, Text) -> Tree -> Tree
toSplice (splice, original) = renamePackageId original splice

renamePackageId :: Text -> Text -> Tree -> Tree
renamePackageId from to = go
  where
    go tree =
      case tree of
        TreeNode "PkgName" [TreeString identity] | identity == from -> TreeNode "PkgName" [TreeString to]
        TreeNode constructor fields -> TreeNode constructor (map go fields)
        _ -> tree

-- | Copy the source file of a module of the original package into the
-- source directory of the splice package. The copy starts with the
-- extensions of the component of the file, because the components of a
-- package can have different default extensions. The @.hs-boot@ file of
-- the module, if there is one, also goes into the directory.
copyOwnModule :: SpliceRequest -> FilePath -> (Text, FilePath) -> IO ()
copyOwnModule request directory (name, path) = do
  let target = directory </> moduleNameDirectory name <.> drop 1 (takeExtension path)
      extensions = maybe [] packageFileExtensions (Map.lookup path (spliceRequestPackageFiles request))
      -- A literate source has its code in marked lines, so it gets no
      -- extra lines.
      pragmas
        | takeExtension path == ".lhs" = ""
        | otherwise = T.pack (concat ["{-# LANGUAGE " <> extension <> " #-}\n" | extension <- extensions])
  createDirectoryIfMissing True (takeDirectory target)
  source <- BS.readFile path
  BS.writeFile target (TE.encodeUtf8 pragmas <> source)
  let boot = replaceExtension path "hs-boot"
  hasBoot <- doesFileExist boot
  when hasBoot (BS.readFile boot >>= BS.writeFile (replaceExtension target "hs-boot"))

-- | The message of a failure. A failure of a file operation also names
-- the file and the operation.
describeFailure :: IOException -> String
describeFailure failure
  | isUserError failure = ioeGetErrorString failure
  | otherwise = show failure

-- | The names that a splice asked @reify@ for, for a failure message.
describeRequests :: [Request] -> String
describeRequests made =
  case [name | RequestReify name _ <- made] of
    [] -> ""
    names -> " (after reify of " <> intercalate ", " (map T.unpack names) <> ")"

-- | Do the request of a splice and give the bytes of the reply. Each
-- request goes to the list. The reply to a report has no bytes. The reply
-- to @reify@ is an @Either String Info@ value.
answerRequest :: (Text, Text) -> (GlobalName -> IO (Either Text Tree)) -> IORef [Request] -> ByteString -> IO ByteString
answerRequest packageIds reify requests bytes =
  case decodeTree bytes >>= requestFromTree . fromSplice packageIds of
    Right made -> do
      modifyIORef' requests (made :)
      case made of
        RequestReport {} -> pure BS.empty
        RequestReify name global -> do
          answer <- maybe (pure (Left ("The compiler can only give the facts of a name with a package and a module, and " <> name <> " does not have them."))) reify global
          pure . encodeTree . toSplice packageIds $ case answer of
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
          buildPlanOptions = splicePlanOptions
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
      installPlanOptions = splicePlanOptions
    }

-- | The plan options of the image build and of the splice package install.
-- The constraints pin the version of each package, so the builds use no
-- lock. A lock would be in the working directory of the user, and the
-- modules of a package run their splices at the same time.
splicePlanOptions :: PlanOptions
splicePlanOptions = defaultPlanOptions {planUseLock = False}

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
        <> ["  other-modules:" | not (null ownModules)]
        <> ["    " <> T.unpack name | (name, _) <- ownModules]
        <> ["  include-dirs:" | not (null includeDirs)]
        <> ["    " <> directory | directory <- includeDirs]
        <> ["  cpp-options:" | not (null cppOptions)]
        <> ["    " <> option | option <- cppOptions]
    )
  where
    extensions = spliceRequestExtensions request
    ownModules = spliceRequestOwnModules request
    -- The settings of the files of the module and of the copied modules.
    files = mapMaybe (\(_, path) -> Map.lookup path (spliceRequestPackageFiles request)) ownModules
    -- A copied file can include a file beside its original with a
    -- relative path, so the directory of each original file is also an
    -- include directory.
    includeDirs = nub (concatMap packageFileIncludeDirs files <> map (takeDirectory . snd) ownModules)
    cppOptions = nub (concatMap packageFileCppOptions files)

renderVersion :: [Int] -> String
renderVersion = intercalate "." . map show
