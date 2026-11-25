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
-- The splice code has no access to the build machine, apart from read
-- access to the directory of the package. See "Aihc.Wasm.Engine".
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
--
-- The user can trust a package with @--unsafe-disable-th-sandbox=PACKAGE@.
-- The splices of that package then run in a native program with no
-- sandbox, not in the WebAssembly engine. 'withNativeSplices' sets this. The splice package then also has an executable, which gives the
-- closures of the splices to "Aihc.TH.NativeEngine". The build installs the
-- package and builds the executable for a native target, with the same
-- store as the build of the module. Thus, the packages that the module
-- imports are already in the store, and the build uses them again. The
-- program speaks the protocol of the engine, so the requests of a splice
-- get the same answers.
module Aihc.Cli.Splice
  ( withSplices,
    withNativeSplices,
    runSplices,
  )
where

import Aihc.Cli.Build (buildWithConfig)
import Aihc.Cli.Install (InstallResult (..), ModuleCompileConfig (..), installWithConfig)
import Aihc.Cli.ModuleProvider (moduleNameDirectory, readTypeArtifactFile)
import Aihc.Cli.Options (BuildOptions (..), InstallOptions (..), PlanOptions (..), defaultPlanOptions)
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest)
import Aihc.Cli.Progress (quietProgress)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact)
import Aihc.Cli.TypeArtifact (TypeArtifact (..))
import Aihc.Hackage.Package (PackageName, Version)
import Aihc.Native (NativeTarget (..), OptimizationLevel (..), hostNativeTarget, nativeTargetStoreDirectory)
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Resolve (OperatorFixity)
import Aihc.Resolve.Types (GlobalName (..), PackageId (..), ResolutionNamespace (..))
import Aihc.Th.Convert (declarationsFromTree, expressionFromTree, patternResultFromTree, typeResultFromTree)
import Aihc.Th.Reify (exportedFixity, fixityTree, reifyConStrictness, reifyInfo, reifyRoles)
import Aihc.Th.Request (Request (..), RequestName (..), lookupNameReply, reportedFailure, requestFromTree)
import Aihc.Th.Splices (FactsAnswer, NameLookup, PackageFile (..), RunSplice, Splice (..), SpliceRequest (..), SpliceResult (..), spliceClosureName, spliceModuleName, spliceModuleText, splicePackageId, splicePackageName, splicePackageVersion, spliceResultsPath)
import Aihc.Th.Wire (Tree (..), decodeTree, encodeTree)
import Aihc.Wasm.Engine (engineGuestPath, linkSharedModule, loadSharedModule, runClosure, runIndexedClosure, startGuest, withEngine, withNativeEngine)
import Control.Exception (IOException, catch, try)
import Control.Monad (forM_, join, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isUpper)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute, removePathForcibly)
import System.FilePath (replaceExtension, takeDirectory, takeExtension, (<.>), (</>))
import System.IO (hPutStrLn, stderr, stdout)
import System.IO.Error (ioeGetErrorString, isUserError)

-- | Make a build run the expression splices of its modules. The builds
-- that 'runSplices' starts do the same, so a package of the image can also
-- have splices.
withSplices :: ModuleCompileConfig -> ModuleCompileConfig
withSplices = withNativeSplices []

-- | 'withSplices' that runs the splices of the packages in the list in a
-- native program with no sandbox, in place of the WebAssembly engine. A
-- package without a version names each version. The program is for the
-- target of the build if the host can run that target. Otherwise, it is
-- for the target of the host.
withNativeSplices :: [(PackageName, Maybe Version)] -> ModuleCompileConfig -> ModuleCompileConfig
withNativeSplices packages config =
  spliceConfig
    SpliceSettings {settingsNativeTarget = nativeEngineTarget (compileTarget config), settingsNativePackages = packages}
    Nothing
    config

-- | What the builds that run splices have in common: the packages whose
-- splices run as native code, and the target of that code.
data SpliceSettings = SpliceSettings
  { -- | The target of the native program. A host that aihc does not
    -- support has no target.
    settingsNativeTarget :: !(Maybe NativeTarget),
    settingsNativePackages :: ![(PackageName, Maybe Version)]
  }

-- | Where the splices of a module run.
data SpliceEngine
  = -- | The WebAssembly engine, with a sandbox.
    WasmEngine
  | -- | A native program for the target, with no sandbox.
    NativeEngine !NativeTarget

-- | The native target of the program that runs splices for a build with
-- the target. The host must run the program, so a build for a different
-- target uses the target of the host.
nativeEngineTarget :: NativeTarget -> Maybe NativeTarget
nativeEngineTarget target
  | target == Llvm = Just Llvm
  | Just target == hostNativeTarget = Just target
  | otherwise = hostNativeTarget

-- | The target of the packages of an engine.
engineTarget :: SpliceEngine -> NativeTarget
engineTarget engine =
  case engine of
    WasmEngine -> Wasm32Wasip3
    NativeEngine target -> target

-- | Make a build run splices with the settings. A splice package of the
-- origin gets the results of the build of the original package.
spliceConfig :: SpliceSettings -> Maybe SpliceOrigin -> ModuleCompileConfig -> ModuleCompileConfig
spliceConfig settings origin config =
  config
    { compileSplices = Just (runSplicesFor settings origin),
      compileNativeSplicePackages = settingsNativePackages settings
    }

-- | Prepare the splices of a request, and give a function that runs one
-- splice to the action. The action runs the splices in the order of the
-- request.
runSplices :: SpliceRequest -> (RunSplice -> IO a) -> IO a
runSplices = runSplicesFor SpliceSettings {settingsNativeTarget = Nothing, settingsNativePackages = []} Nothing

-- | The original package of a splice package. The splice package has
-- copies of modules of the original package.
data SpliceOrigin = SpliceOrigin
  { -- | The name of the splice package.
    originSplicePackage :: !Text,
    -- | The directory of the original package. A splice in a copy reads the
    -- files of the original package.
    originPackageRoot :: !FilePath,
    -- | The package id of the names of the original package.
    originPackageId :: !Text,
    -- | The file with the splice results of each copied module that has
    -- splices, from the build of the original package.
    originResults :: !(Map Text FilePath),
    -- | The engine of the original package. A copy runs its splices in the
    -- same engine.
    originEngine :: !SpliceEngine
  }

-- | 'runSplices' for the build of a splice package. A request of the
-- splice package gets the directory of the original package. If the build
-- of the original package ran the splices of the module, the copy gets
-- the same results. Thus, the copy does not make a splice package of its
-- own, and splice packages do not go in each other.
runSplicesFor :: SpliceSettings -> Maybe SpliceOrigin -> SpliceRequest -> (RunSplice -> IO a) -> IO a
runSplicesFor settings origin given action =
  run `catch` \failure ->
    ioError (userError ("Template Haskell failed in module " <> T.unpack (spliceRequestModule request) <> ": " <> describeFailure failure <> unavailableNote))
  where
    ownOrigin = case origin of
      Just found | spliceRequestPackage given == originSplicePackage found -> Just found
      _ -> Nothing
    request = maybe given (\found -> given {spliceRequestPackageRoot = originPackageRoot found}) ownOrigin
    -- The engine of the request: the engine of the original package for a
    -- copy, and a native program for a package that the user trusts.
    chosenEngine =
      case ownOrigin of
        Just found -> Right (originEngine found)
        Nothing
          | spliceRequestNative request -> maybe (Left "--unsafe-disable-th-sandbox needs a native target for this host, and aihc does not support this host") (Right . NativeEngine) (settingsNativeTarget settings)
          | otherwise -> Right WasmEngine
    root = spliceRequestBuildRoot request
    resultsPath = spliceResultsPath request
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
      createDirectoryIfMissing True root
      removePathForcibly resultsPath
      recorded <- case ownOrigin >>= Map.lookup (spliceRequestModule request) . originResults of
        Just path -> do
          exists <- doesFileExist path
          pure (if exists then Just path else Nothing)
        Nothing -> pure Nothing
      results <- newIORef Map.empty
      outcome <- maybe (runEngine results) (replay results) recorded
      trees <- readIORef results
      BS.writeFile resultsPath (encodeTree (TreeNode "Results" [TreeNode "Result" [TreeInteger (toInteger index), tree] | (index, tree) <- Map.toList trees]))
      pure outcome
    -- The splice results of the build of the original package, with the
    -- package id of this package in place of the id of that package.
    replay results path = do
      bytes <- BS.readFile path
      recorded <- either (ioError . userError . T.unpack) pure (decodeTree bytes >>= resultTrees)
      let from = maybe (spliceRequestPackageId request) originPackageId ownOrigin
          renamed = Map.map (renamePackageId from (spliceRequestPackageId request)) recorded
      action $ \index _ _ _ -> do
        splice <- spliceAt index
        tree <- maybe (ioError (userError ("The recorded results in " <> path <> " have no result for splice " <> show index <> "."))) pure (Map.lookup index renamed)
        modifyIORef' results (Map.insert index tree)
        either (ioError . userError . T.unpack) pure (convertResult splice tree)
    spliceAt index =
      maybe (ioError (userError ("The request has no splice " <> show index <> "."))) pure (lookup index (zip [0 ..] (spliceRequestSplices request)))
    runEngine results = do
      engine <- either (ioError . userError) pure chosenEngine
      absolute <- absoluteRequest request
      createDirectoryIfMissing True (packageDirectory </> "src")
      writeFile (packageDirectory </> T.unpack package <> ".cabal") (splicePackageDescription engine absolute)
      TIO.writeFile (packageDirectory </> "src" </> T.unpack spliceModuleName <> ".hs") (spliceModuleText request)
      mapM_ (copyOwnModule absolute (packageDirectory </> "src")) (spliceRequestOwnModules absolute)
      case engine of
        WasmEngine -> pure ()
        NativeEngine _ -> do
          createDirectoryIfMissing True (packageDirectory </> "guest")
          TIO.writeFile (packageDirectory </> "guest" </> "Main.hs") (nativeGuestText request)
      packageRoot <- makeAbsolute (spliceRequestPackageRoot request)
      let nested =
            SpliceOrigin
              { originSplicePackage = package,
                originPackageRoot = packageRoot,
                originPackageId = spliceRequestPackageId request,
                originResults = Map.fromList (spliceRequestOwnResults absolute),
                originEngine = engine
              }
          withOriginalFiles = spliceConfig settings (Just nested)
      installed <- installWithConfig (quietProgress stdout) (packageOptions engine request packageDirectory) withOriginalFiles
      packages <- packageDirectories (spliceRequestStoreRoot request </> nativeTargetStoreDirectory (engineTarget engine)) (installStorePath installed)
      storeFacts <- newReifier (Map.map snd packages)
      storeFixity <- newFixityReader (Map.map snd packages)
      let fromGuest = fromSplice packageIds (primPackageId packages)
          toGuest = toSplice packageIds
      withSpliceEngine engine settings request packageDirectory packageRoot installed withOriginalFiles $ \runIndex ->
        action $ \index lookupName localFacts localFixity -> do
          splice <- spliceAt index
          let reify answer global = localFacts answer global >>= maybe (storeFacts answer global) pure
              fixity global = localFixity global >>= maybe (storeFixity global) (pure . Right)
          requests <- newIORef []
          let location = spliceLocation request (join (lookup index (zip [0 ..] (spliceRequestSpans request))))
          outcome <- try (runIndex (answerRequest fromGuest toGuest lookupName reify fixity location requests) index)
          made <- reverse <$> readIORef requests
          forM_ [message | RequestReport False message <- made] $ \message ->
            hPutStrLn stderr ("warning: Template Haskell in module " <> T.unpack (spliceRequestModule request) <> ": " <> T.unpack message)
          -- A report of an error is the cause of a failure of the closure,
          -- so the report comes before the failure itself.
          case (reportedFailure made, outcome) of
            (Just message, _) -> ioError (userError (T.unpack message))
            (Nothing, Left failure) -> ioError (userError ("splice " <> show index <> describeRequests made <> ": " <> describeFailure failure))
            (Nothing, Right [message]) -> do
              tree <- either (ioError . userError . T.unpack) (pure . fromGuest) (decodeTree message)
              modifyIORef' results (Map.insert index tree)
              either (ioError . userError . T.unpack) pure (convertResult splice tree)
            (Nothing, Right messages) -> ioError (userError ("a splice sent " <> show (length messages) <> " messages"))

-- | Start the engine of a request and give the action a function that
-- runs the splice at an index. The function gets the function that answers
-- the requests of the splice.
--
-- The WebAssembly engine has an image with the packages of the module, and
-- loads the splice package as a shared module. A native engine is the
-- executable of the splice package, which runs in the directory of the
-- original package.
withSpliceEngine ::
  SpliceEngine ->
  SpliceSettings ->
  SpliceRequest ->
  FilePath ->
  FilePath ->
  InstallResult ->
  (ModuleCompileConfig -> ModuleCompileConfig) ->
  (((ByteString -> IO ByteString) -> Int -> IO [ByteString]) -> IO a) ->
  IO a
withSpliceEngine engine settings request packageDirectory packageRoot installed adjust action =
  case engine of
    WasmEngine -> do
      image <- buildImage settings request
      let shared = spliceRequestBuildRoot request </> "splices.wasm"
      linkSharedModule shared [] [installStorePath installed </> "lib" </> "lib" <> T.unpack (splicePackageName request) <> ".a"]
      withEngine (Just packageRoot) image $ \running -> do
        startGuest running
        loadSharedModule running shared
        action (\answer index -> runClosure running answer (splicePackageId request) spliceModuleName (spliceClosureName index))
    NativeEngine target -> do
      programs <- buildWithConfig (quietProgress stdout) (nativeProgramOptions target request packageDirectory) adjust
      program <- case programs of
        [built] -> makeAbsolute built
        _ -> ioError (userError "the build of the splice program gave no executable")
      withNativeEngine (Just packageRoot) program $ \running ->
        action (runIndexedClosure running)

-- | The result of a splice from its tree.
convertResult :: Splice -> Tree -> Either Text SpliceResult
convertResult splice tree =
  case splice of
    ExpressionSplice _ -> ExpressionResult <$> expressionFromTree tree
    TypeSplice _ -> TypeResult <$> typeResultFromTree tree
    PatternSplice _ -> PatternResult <$> patternResultFromTree tree
    DeclarationSplice _ -> DeclarationResult <$> declarationsFromTree tree

-- | The trees of a file of splice results, by the position of the splice.
resultTrees :: Tree -> Either Text (Map Int Tree)
resultTrees tree =
  case tree of
    TreeNode "Results" entries -> Map.fromList <$> mapM entry entries
    _ -> Left "The file of splice results is not correct."
  where
    entry value =
      case value of
        TreeNode "Result" [TreeInteger index, result] -> Right (fromInteger index, result)
        _ -> Left "The file of splice results is not correct."

-- | The request with absolute paths for the source files and the include
-- directories of the package. The Cabal file of the splice package names
-- them, and a relative path in that file starts at the splice package.
absoluteRequest :: SpliceRequest -> IO SpliceRequest
absoluteRequest request = do
  ownModules <- mapM (\(name, path) -> (name,) <$> makeAbsolute path) (spliceRequestOwnModules request)
  ownResults <- mapM (\(name, path) -> (name,) <$> makeAbsolute path) (spliceRequestOwnResults request)
  files <-
    Map.fromList
      <$> mapM
        ( \(path, file) -> do
            absolute <- makeAbsolute path
            includeDirs <- mapM makeAbsolute (packageFileIncludeDirs file)
            pure (absolute, file {packageFileIncludeDirs = includeDirs})
        )
        [entry | entry@(path, _) <- Map.toList (spliceRequestPackageFiles request), path `elem` map snd (spliceRequestOwnModules request)]
  pure request {spliceRequestOwnModules = ownModules, spliceRequestOwnResults = ownResults, spliceRequestPackageFiles = files}

-- | Change the package id of the splice package to the package id of the
-- original package, in each name of a tree from the splice. The first id
-- of the pair is the id of the splice package.
--
-- The library gives the package @ghc-prim@ to some built-in names, as GHC
-- does. An example is the name that @tupleTypeName@ gives. The function
-- changes that package to the given id of the @aihc-prim@ package, which
-- has these names.
fromSplice :: (Text, Text) -> Maybe PackageId -> Tree -> Tree
fromSplice (splice, original) prim =
  maybe id (renamePackageId "ghc-prim" . packageIdText) prim . renamePackageId splice original

-- | The package id of the @aihc-prim@ package, if the splice package
-- depends on it.
primPackageId :: Map PackageId (Text, FilePath) -> Maybe PackageId
primPackageId packages =
  case [identity | (identity, (name, _)) <- Map.toList packages, name == "aihc-prim"] of
    identity : _ -> Just identity
    [] -> Nothing

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
  case [name | request <- made, Just name <- [reifiedName request]] of
    [] -> ""
    names -> " (after reify of " <> intercalate ", " (map T.unpack names) <> ")"
  where
    reifiedName request =
      case request of
        RequestReify name _ -> Just name
        RequestReifyRoles name _ -> Just name
        RequestReifyConStrictness name _ -> Just name
        RequestReifyFixity name _ -> Just name
        RequestReport {} -> Nothing
        RequestLocation -> Nothing
        RequestLookupName {} -> Nothing

-- | Do the request of a splice and give the bytes of the reply. Each
-- request goes to the list. The reply to a report has no bytes. The reply
-- to @reify@ is an @Either String Info@ value. The reply to @reifyRoles@ is
-- an @Either String [Role]@ value. The reply to @reifyConStrictness@ is an
-- @Either String [DecidedStrictness]@ value. The reply to @reifyFixity@ is
-- an @Either String (Maybe Fixity)@ value. A name that the compiler cannot
-- find has no fixity declaration that the compiler knows, so it gives
-- 'Nothing'. The reply to @location@ is the @Loc@ value of the splice.
answerRequest :: (Tree -> Tree) -> (Tree -> Tree) -> NameLookup -> (FactsAnswer -> GlobalName -> IO (Either Text Tree)) -> (GlobalName -> IO (Either Text (Maybe OperatorFixity))) -> Tree -> IORef [Request] -> ByteString -> IO ByteString
answerRequest fromGuest toGuest lookupName reify fixity location requests bytes =
  case decodeTree bytes >>= requestFromTree . fromGuest of
    Right made -> do
      modifyIORef' requests (made :)
      case made of
        RequestReport {} -> pure BS.empty
        RequestReify name global -> facts reifyInfo name global
        RequestReifyRoles name global -> facts reifyRoles name global
        RequestReifyConStrictness name global -> facts reifyConStrictness name global
        RequestReifyFixity name requested -> do
          answer <- case requested of
            RequestGlobal global -> fixity global
            RequestScoped scoped -> maybe (pure (Right Nothing)) fixity (scopedName name scoped)
            RequestUnknown -> pure (Right Nothing)
          pure (encodeReply (fixityTree <$> answer))
        RequestLocation -> pure (encodeTree location)
        RequestLookupName isType scoped ->
          let namespace = if isType then ResolutionNamespaceType else ResolutionNamespaceTerm
           in pure (encodeTree (toGuest (lookupNameReply (lookupName namespace scoped))))
    Left message -> ioError (userError (T.unpack message))
  where
    facts question name requested =
      case requested of
        RequestGlobal global -> reply (reify question global)
        RequestScoped scoped ->
          case scopedName name scoped of
            Just global -> reply (reify question global)
            Nothing -> reply (pure (Left (name <> " is not in scope at the splice.")))
        RequestUnknown -> reply (pure (Left ("The compiler can only give the facts of a name with a package and a module, or of a name in scope at the splice, and " <> name <> " is not such a name.")))
    scopedName name scoped = listToMaybe (mapMaybe (`lookupName` scoped) (guessedNamespaces name))
    reply ask = encodeReply <$> ask
    encodeReply answer =
      encodeTree . toGuest $ case answer of
        Right value -> TreeNode "Right" [value]
        Left message -> TreeNode "Left" [TreeString message]

-- | The namespaces where the compiler looks for a name with no package, in
-- order. As with GHC, a name that starts with an upper-case letter or a
-- colon is first a type constructor or a class, and then a data
-- constructor.
guessedNamespaces :: Text -> [ResolutionNamespace]
guessedNamespaces name =
  case T.uncons name of
    Just (first, _)
      | isUpper first || first == ':' -> [ResolutionNamespaceType, ResolutionNamespaceTerm]
    _ -> [ResolutionNamespaceTerm]

-- | The @Loc@ value of a splice: the source file, the package id, the
-- module, and the start and the end of the splice. A splice with no source
-- span gets the position 1:1. GHC gives the column after the last
-- character as the end, as the source span does.
spliceLocation :: SpliceRequest -> Maybe Syntax.SourceSpan -> Tree
spliceLocation request span' =
  TreeNode
    "Loc"
    [ TreeString (T.pack (spliceRequestSourceFile request)),
      TreeString (spliceRequestPackageId request),
      TreeString (spliceRequestModule request),
      position start,
      position end
    ]
  where
    (start, end) =
      case span' of
        Just (Syntax.SourceSpan _ startLine startColumn endLine endColumn _ _) -> ((startLine, startColumn), (endLine, endColumn))
        Nothing -> ((1, 1), (1, 1))
    position (line, column) = TreeNode "(,)" [TreeInteger (toInteger line), TreeInteger (toInteger column)]

-- | The name and the store directory of each package below a package, by
-- the package id that names of the package have. The manifest of a package names the
-- store directories of the packages that it depends on. A package that is
-- not in the store, such as a local package, is not in the result.
packageDirectories :: FilePath -> FilePath -> IO (Map PackageId (Text, FilePath))
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
              go (Map.insert (PackageId (packageManifestUnitId dependency)) (packageManifestName dependency, directory) found) (packageManifestDependencies dependency <> rest)
        _ -> go found rest
    readManifest directory = do
      let path = packageManifestPath directory
      exists <- doesFileExist path
      if exists
        then readPackageManifest path >>= either (\message -> ioError (userError ("Invalid package manifest " <> path <> ": " <> message))) (pure . Just)
        else pure Nothing

-- | Give the facts of a name from the type artifact of its module. The
-- reifier reads each artifact one time.
newReifier :: Map PackageId FilePath -> IO (FactsAnswer -> GlobalName -> IO (Either Text Tree))
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
  pure $ \answer global ->
    case Map.lookup (globalNamePackage global) packages of
      Nothing -> pure (Left ("The splice cannot use the package " <> packageIdText (globalNamePackage global) <> " of " <> globalNameText global <> "."))
      Just directory -> do
        artifact <- artifactOf directory (globalNameModule global)
        pure $ case artifact of
          Nothing -> Left ("The package " <> packageIdText (globalNamePackage global) <> " has no type facts of the module " <> globalNameModule global <> ".")
          Just found -> answer (typeArtifactInterface found) global

-- | Give the fixity of a name from the resolve artifact of its module.
-- The reader reads each artifact one time.
newFixityReader :: Map PackageId FilePath -> IO (GlobalName -> IO (Either Text (Maybe OperatorFixity)))
newFixityReader packages = do
  artifacts <- newIORef Map.empty
  let artifactOf directory home = do
        cached <- Map.lookup (directory, home) <$> readIORef artifacts
        case cached of
          Just artifact -> pure artifact
          Nothing -> do
            let path = directory </> moduleNameDirectory home </> "resolve.cbor"
            exists <- doesFileExist path
            artifact <-
              if exists
                then either (\message -> ioError (userError ("Invalid resolve artifact " <> path <> ": " <> message))) (pure . Just) . decodeResolveArtifact =<< BS.readFile path
                else pure Nothing
            atomicModifyIORef' artifacts (\known -> (Map.insert (directory, home) artifact known, ()))
            pure artifact
  pure $ \global ->
    case Map.lookup (globalNamePackage global) packages of
      Nothing -> pure (Left ("The splice cannot use the package " <> packageIdText (globalNamePackage global) <> " of " <> globalNameText global <> "."))
      Just directory -> do
        artifact <- artifactOf directory (globalNameModule global)
        pure $ case artifact of
          Nothing -> Left ("The package " <> packageIdText (globalNamePackage global) <> " has no resolve facts of the module " <> globalNameModule global <> ".")
          Just found -> Right (exportedFixity (resolveArtifactExports found) global)

-- | Build the image of a request and give its path. A package of the
-- image that the user trusts runs its splices as native code.
buildImage :: SpliceSettings -> SpliceRequest -> IO FilePath
buildImage settings request = do
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
      (spliceConfig settings Nothing)
  case images of
    [image] -> pure image
    _ -> ioError (userError "the image build gave no image")
  where
    constraint (name, version) = T.unpack name <> " == " <> renderVersion version

-- | The options that build the executable of the splice package for a
-- native engine. The build root is the build root of the install of the
-- package, so the build uses the modules of the install again.
nativeProgramOptions :: NativeTarget -> SpliceRequest -> FilePath -> BuildOptions
nativeProgramOptions target request directory =
  BuildOptions
    { buildInput = directory,
      buildSourceDirectories = [],
      buildPackageConstraints = [],
      buildTarget = target,
      buildStoreRoot = Just (spliceRequestStoreRoot request),
      buildBuildRoot = Just (spliceRequestBuildRoot request </> "package-build"),
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
      buildWasmImage = False,
      buildVerbose = False,
      buildPrintTimings = False,
      buildOutput = Just (spliceRequestBuildRoot request </> "bin"),
      buildExecutables = [nativeGuestName],
      buildPlanOptions = splicePlanOptions
    }

-- | The options that install the splice package: for a shared module of
-- the WebAssembly engine, or for the executable of a native engine.
packageOptions :: SpliceEngine -> SpliceRequest -> FilePath -> InstallOptions
packageOptions engine request directory =
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
      installPositionIndependent = case engine of
        WasmEngine -> True
        NativeEngine _ -> False,
      installVerbose = False,
      installPrintTimings = False,
      installTarget = engineTarget engine,
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
splicePackageDescription :: SpliceEngine -> SpliceRequest -> String
splicePackageDescription engine request =
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
        <> executable
    )
  where
    -- The executable of a native engine. It depends on the packages of the
    -- library, so its main module can use the Prelude.
    executable =
      case engine of
        WasmEngine -> []
        NativeEngine _ ->
          [ "",
            "executable " <> nativeGuestName,
            "  main-is: Main.hs",
            "  hs-source-dirs: guest",
            "  default-language: Haskell2010",
            "  build-depends:",
            "    " <> T.unpack (splicePackageName request) <> ","
          ]
            <> ["    " <> T.unpack name <> " ==" <> renderVersion version <> "," | (name, version) <- spliceRequestDependencies request]
            <> ["    template-haskell," | "template-haskell" `notElem` map fst (spliceRequestDependencies request)]
    extensions = spliceRequestExtensions request
    ownModules = spliceRequestOwnModules request
    -- The settings of the files of the module and of the copied modules.
    files = mapMaybe (\(_, path) -> Map.lookup path (spliceRequestPackageFiles request)) ownModules
    -- A copied file can include a file beside its original with a
    -- relative path, so the directory of each original file is also an
    -- include directory.
    includeDirs = nub (concatMap packageFileIncludeDirs files <> map (takeDirectory . snd) ownModules)
    cppOptions = nub (concatMap packageFileCppOptions files)

-- | The name of the executable of a native engine.
nativeGuestName :: String
nativeGuestName = "aihc-splice-guest"

-- | The main module of the executable of a native engine. It gives the
-- closure of each splice to "Aihc.TH.NativeEngine", in the order of the
-- splices, so the index of a closure is the index of its splice.
nativeGuestText :: SpliceRequest -> Text
nativeGuestText request =
  T.unlines
    ( [ "module Main (main) where",
        "",
        "import qualified " <> spliceModuleName,
        "import qualified Aihc.TH.NativeEngine",
        "",
        "main :: IO ()",
        "main =",
        "  Aihc.TH.NativeEngine.serveSplices"
      ]
        <> closureLines
    )
  where
    names = [spliceModuleName <> "." <> spliceClosureName index | index <- [0 .. length (spliceRequestSplices request) - 1]]
    closureLines =
      case names of
        [] -> ["    []"]
        first : rest -> ["    [ " <> first] <> ["    , " <> name | name <- rest] <> ["    ]"]

renderVersion :: [Int] -> String
renderVersion = intercalate "." . map show
