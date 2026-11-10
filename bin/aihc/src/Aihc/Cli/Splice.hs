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
-- 4. Run the closure of each splice, and convert each result to the
--    surface syntax.
--
-- The splice code has no access to the build machine. See
-- "Aihc.Wasm.Engine".
--
-- A splice cannot use a module of its own package at this time.
module Aihc.Cli.Splice
  ( withSplices,
    runSplices,
  )
where

import Aihc.Cli.Build (buildWithConfig)
import Aihc.Cli.Install (InstallResult (..), ModuleCompileConfig (..), installWithConfig)
import Aihc.Cli.Options (BuildOptions (..), InstallOptions (..), defaultPlanOptions)
import Aihc.Cli.Progress (quietProgress)
import Aihc.Native (NativeTarget (..), OptimizationLevel (..))
import Aihc.Th.Convert (declarationsFromTree, expressionFromTree)
import Aihc.Th.Splices (Splice (..), SpliceRequest (..), SpliceResult (..), spliceClosureName, spliceModuleName, spliceModuleText)
import Aihc.Th.Wire (decodeTree)
import Aihc.Wasm.Engine (engineGuestPath, linkSharedModule, loadSharedModule, runClosure, startGuest, withEngine)
import Control.Exception (IOException, catch)
import Control.Monad (forM)
import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (stdout)
import System.IO.Error (ioeGetErrorString)

-- | Make a build run the expression splices of its modules. The builds
-- that 'runSplices' starts do the same, so a package of the image can also
-- have splices.
withSplices :: ModuleCompileConfig -> ModuleCompileConfig
withSplices config = config {compileSplices = Just runSplices}

-- | The result of each splice of a request, in the order of the request.
runSplices :: SpliceRequest -> IO [SpliceResult]
runSplices request =
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
      withEngine image $ \engine -> do
        startGuest engine
        loadSharedModule engine shared
        forM (zip [0 ..] (spliceRequestSplices request)) $ \(index, splice) -> do
          messages <- runClosure engine (package <> "-" <> splicePackageVersion) spliceModuleName (spliceClosureName index)
          let convert tree =
                case splice of
                  ExpressionSplice _ -> ExpressionResult <$> expressionFromTree tree
                  DeclarationSplice _ -> DeclarationResult <$> declarationsFromTree tree
          case messages of
            [message] -> either (ioError . userError . T.unpack) pure (decodeTree message >>= convert)
            _ -> ioError (userError ("a splice sent " <> show (length messages) <> " messages"))

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
