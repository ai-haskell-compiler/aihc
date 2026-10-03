{-# LANGUAGE OverloadedStrings #-}

-- | The final step of @build@: link the objects of an executable and the
-- packages below it, or write them to a link bundle for @link-exe@.
module Aihc.Cli.Link
  ( LinkBundle (..),
    linkBundleManifestPath,
    linkCompiledExecutable,
    runLinkExe,
  )
where

import Aihc.Cli.Backend (compileEntryObject)
import Aihc.Cli.Install
  ( CompiledExecutable (..),
    InstallResult (..),
    InstalledPackage (..),
    ModuleCompileConfig (..),
    archiveHasMembers,
    moduleObjectPaths,
    packageLinkArguments,
  )
import Aihc.Cli.Lto (compileLtoProgram, moduleCorePath)
import Aihc.Cli.Options (LinkExeOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..))
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Native (NativeTarget (..), WasmSysroot (..), backendCompiler, cxxStandardLibraryArguments, executableLinkArguments, llvmLto, llvmLtoLinkArguments, parseNativeTarget, readWasmClangProcessWithExitCode, renderNativeTarget, wasmSysroot)
import Aihc.Wasm (wasip3WorldPath)
import Control.Exception (bracket)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Directory
  ( copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

-- | Turn a compiled executable and the packages below it into the
-- executable, or into a link bundle when the link is deferred. The runtime
-- is the @aihc-rts@ package among the packages; the entry unit is generated
-- in the build directory of the executable, where a @--lto@ build also
-- writes the program object.
linkCompiledExecutable :: ModuleCompileConfig -> Bool -> FilePath -> FilePath -> CompiledExecutable -> IO ()
linkCompiledExecutable compileConfig noLink buildRoot output executable = do
  let target = compileTarget compileConfig
      packages = compiledPackages executable
      cCompileInfo = compiledCCompileInfo executable
  validatePackageNames packages
  mapM_ requirePackageArchive packages
  createDirectoryIfMissing True buildRoot
  let entry = buildRoot </> "entry.o"
      lto = compileLto compileConfig
  compileEntryObject lto target buildRoot entry
  -- A @--lto@ build compiles the System FC of every module of the program,
  -- from the packages and the executable alike, into one object. The
  -- package archives then hold only their C and capi wrapper objects.
  programObjects <-
    if lto
      then do
        let corePaths =
              [ moduleCorePath target (packageRoot package) name
              | package <- packages,
                name <- packageManifestCompiledModules (installedManifest package)
              ]
                <> [moduleCorePath target buildRoot name | name <- compiledModuleNames executable]
        object <- compileLtoProgram compileConfig buildRoot corePaths
        pure [object]
      else pure []
  createDirectoryIfMissing True (takeDirectory output)
  let orderedPackages = linkOrderedPackages packages
  cObjects <- fmap concat (mapM packageCObjects orderedPackages)
  -- A link-time optimized build of the LLVM target links no archive: the
  -- archive of each package holds bitcode without a symbol table, so the
  -- link takes the wrapper objects of the package as objects; see
  -- 'llvmLto'. Its C objects are among the objects already.
  wrapperObjects <-
    if llvmLto target lto
      then fmap concat (mapM (packageWrapperObjects target) orderedPackages)
      else pure []
  let objects = programObjects <> compiledModuleObjects executable <> [entry] <> compiledCObjects executable <> cObjects <> wrapperObjects
  -- A package whose archive holds no member is left out of the link: a
  -- @--lto@ build leaves the archive of a package without C sources empty,
  -- and so does a package whose modules are all empty standins.
  archives <-
    if llvmLto target lto
      then pure []
      else filterM archiveHasMembers (map packageArchive orderedPackages)
  -- A package with cxx-sources says so in its manifest, and its objects
  -- need the C++ standard library however the program reaches them. So do
  -- the objects of the executable itself.
  let cxxStdLib =
        not (null (HackageCabal.cCompileCxxSources cCompileInfo))
          || any (packageManifestCxxStdLib . installedManifest) orderedPackages
      -- The system libraries come after every archive, so the members of
      -- each archive can resolve their symbols from them.
      linkArguments =
        nub
          ( packageLinkArguments target cCompileInfo
              <> concatMap (map T.unpack . packageManifestLinkArguments . installedManifest) orderedPackages
          )
      -- A @--lto@ build of the LLVM target has compiled every object to
      -- bitcode, and the link is where the whole program is optimized.
      libraries =
        LinkLibraries
          { linkCxxStdLib = cxxStdLib,
            linkArguments,
            linkLtoArguments = llvmLtoLinkArguments target lto (compileOptimization compileConfig)
          }
  if noLink
    then writeLinkBundle target output libraries objects archives
    else linkExecutable target output libraries objects archives

-- | The directory of an installed package: in the store, or in the build
-- directory for a local package.
packageRoot :: InstalledPackage -> FilePath
packageRoot = installStorePath . installedResult

-- | The packages below an executable hold one build of each package name.
validatePackageNames :: [InstalledPackage] -> IO ()
validatePackageNames packages =
  forM_ (Map.toList packagesByName) $ \(name, builds) ->
    case builds of
      [_] -> pure ()
      _ -> ioError (userError ("The dependency plan selects more than one build of " <> T.unpack name))
  where
    packagesByName =
      Map.fromListWith
        (<>)
        [ (packageManifestName (installedManifest package), [package])
        | package <- packages
        ]

-- | Everything the final link of an executable consumes, with paths relative
-- to the bundle directory. The bundle is self-contained, so a machine that
-- cannot run the compiler, or that lacks the linker for the target the
-- compiler ran on, can still produce the executable with @link-exe@.
--
-- Schema 5 adds the arguments of a link-time optimized link, whose inputs
-- are bitcode. Schema 4 adds the arguments that link the system libraries
-- of the packages. Schema 3 adds whether the link needs the C++ standard
-- library. Schema 2 lists objects and archives only. Schema 1 also named an
-- entry and a runtime archive, which are now an object among the objects
-- and the archive and C objects of the @aihc-rts@ package.
data LinkBundle = LinkBundle
  { linkBundleTarget :: !NativeTarget,
    -- | An input was compiled from @cxx-sources@, so the link adds the
    -- C++ standard library of the target.
    linkBundleCxxStdLib :: !Bool,
    -- | The arguments that link the system libraries the packages name.
    linkBundleLinkArguments :: ![String],
    -- | The arguments of a link whose inputs are bitcode; see
    -- 'Aihc.Native.llvmLtoLinkArguments'. Empty for every other link.
    linkBundleLtoArguments :: ![String],
    linkBundleObjects :: ![FilePath],
    linkBundleArchives :: ![FilePath]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON LinkBundle where
  toJSON bundle =
    Aeson.object
      [ "schemaVersion" .= (5 :: Int),
        "target" .= renderNativeTarget (linkBundleTarget bundle),
        "cxxStdLib" .= linkBundleCxxStdLib bundle,
        "linkArguments" .= linkBundleLinkArguments bundle,
        "ltoArguments" .= linkBundleLtoArguments bundle,
        "objects" .= linkBundleObjects bundle,
        "archives" .= linkBundleArchives bundle
      ]

instance Aeson.FromJSON LinkBundle where
  parseJSON = Aeson.withObject "LinkBundle" $ \object -> do
    schemaVersion <- object .: "schemaVersion"
    case schemaVersion :: Int of
      2 -> do
        target <- object .: "target" >>= either fail pure . parseNativeTarget
        LinkBundle target False [] []
          <$> object .: "objects"
          <*> object .: "archives"
      3 -> do
        target <- object .: "target" >>= either fail pure . parseNativeTarget
        LinkBundle target
          <$> object .: "cxxStdLib"
          <*> pure []
          <*> pure []
          <*> object .: "objects"
          <*> object .: "archives"
      4 -> do
        target <- object .: "target" >>= either fail pure . parseNativeTarget
        LinkBundle target
          <$> object .: "cxxStdLib"
          <*> object .: "linkArguments"
          <*> pure []
          <*> object .: "objects"
          <*> object .: "archives"
      5 -> do
        target <- object .: "target" >>= either fail pure . parseNativeTarget
        LinkBundle target
          <$> object .: "cxxStdLib"
          <*> object .: "linkArguments"
          <*> object .: "ltoArguments"
          <*> object .: "objects"
          <*> object .: "archives"
      _ -> fail "unsupported link bundle schema"

linkBundleManifestPath :: FilePath -> FilePath
linkBundleManifestPath bundle = bundle </> "link.json"

-- | Copy the link inputs into the bundle directory and describe them in the
-- manifest. Each copy carries its position in the link order as a prefix, so
-- inputs from different packages that share a file name never collide.
writeLinkBundle :: NativeTarget -> FilePath -> LinkLibraries -> [FilePath] -> [FilePath] -> IO ()
writeLinkBundle target bundle libraries objects archives = do
  let inputs = bundle </> "inputs"
  createDirectoryIfMissing True inputs
  copied <- forM (zip [0 :: Int ..] (objects <> archives)) $ \(index, source) -> do
    let name = padIndex index <> "-" <> takeFileName source
    copyFile source (inputs </> name)
    pure ("inputs" </> name)
  let (copiedObjects, copiedArchives) = splitAt (length objects) copied
  BL.writeFile
    (linkBundleManifestPath bundle)
    ( Aeson.encode
        LinkBundle
          { linkBundleTarget = target,
            linkBundleCxxStdLib = linkCxxStdLib libraries,
            linkBundleLinkArguments = linkArguments libraries,
            linkBundleLtoArguments = linkLtoArguments libraries,
            linkBundleObjects = copiedObjects,
            linkBundleArchives = copiedArchives
          }
    )
  where
    padIndex index = replicate (4 - length (show index)) '0' <> show index

runLinkExe :: LinkExeOptions -> IO ()
runLinkExe options = do
  let bundle = linkExeBundle options
      manifest = linkBundleManifestPath bundle
      output = linkExeOutputFile options
  exists <- doesFileExist manifest
  unless exists (ioError (userError ("No link bundle manifest at " <> manifest)))
  decoded <- Aeson.eitherDecode <$> BL.readFile manifest
  LinkBundle {linkBundleTarget, linkBundleCxxStdLib, linkBundleLinkArguments, linkBundleLtoArguments, linkBundleObjects, linkBundleArchives} <-
    either (ioError . userError . (("Invalid link bundle manifest " <> manifest <> ": ") <>)) pure decoded
  createDirectoryIfMissing True (takeDirectory output)
  linkExecutable
    linkBundleTarget
    output
    LinkLibraries {linkCxxStdLib = linkBundleCxxStdLib, linkArguments = linkBundleLinkArguments, linkLtoArguments = linkBundleLtoArguments}
    (map (bundle </>) linkBundleObjects)
    (map (bundle </>) linkBundleArchives)

-- | Put a package before the packages it depends on.
-- GNU ld searches each archive once, so a later archive cannot satisfy an
-- earlier archive.
linkOrderedPackages :: [InstalledPackage] -> [InstalledPackage]
linkOrderedPackages packages =
  reverse (snd (foldl visit (Set.empty, []) packages))
  where
    byIdentity =
      Map.fromList
        [ (packageIdentity package, package)
        | package <- packages
        ]
    packageIdentity = packageManifestIdentity . installedManifest
    visit (seen, ordered) package
      | Set.member (packageIdentity package) seen = (seen, ordered)
      | otherwise =
          let seenSelf = Set.insert (packageIdentity package) seen
              dependencies =
                mapMaybe
                  (`Map.lookup` byIdentity)
                  (packageManifestDependencies (installedManifest package))
              (seenDeps, orderedDeps) = foldl visit (seenSelf, ordered) dependencies
           in (seenDeps, orderedDeps ++ [package])

packageCObjects :: InstalledPackage -> IO [FilePath]
packageCObjects package = do
  let directory = packageRoot package </> "cbits"
  exists <- doesDirectoryExist directory
  if not exists
    then pure []
    else do
      names <- listDirectory directory
      pure (sortOn id [directory </> name | name <- names, ".o" `isSuffixOf` name])

-- | The objects of the capi wrappers of the modules of an installed
-- package: what its archive holds besides the C objects when the build
-- stops at System FC.
packageWrapperObjects :: NativeTarget -> InstalledPackage -> IO [FilePath]
packageWrapperObjects target package =
  moduleObjectPaths False (packageRoot package) target (packageManifestCompiledModules (installedManifest package))

packageArchive :: InstalledPackage -> FilePath
packageArchive package =
  packageRoot package
    </> "lib"
    </> "lib"
      <> T.unpack (packageManifestName (installedManifest package))
      <> ".a"

requirePackageArchive :: InstalledPackage -> IO ()
requirePackageArchive package = do
  let archive = packageArchive package
  exists <- doesFileExist archive
  unless exists $
    ioError
      ( userError
          ( "The library "
              <> T.unpack (packageManifestName (installedManifest package))
              <> " is not compiled for the target: "
              <> archive
          )
      )

-- | The libraries that a link adds after the objects and archives of the
-- program.
data LinkLibraries = LinkLibraries
  { -- | An input was compiled from @cxx-sources@, so the link adds the
    -- C++ standard library of the target.
    linkCxxStdLib :: !Bool,
    -- | The arguments that link the system libraries the packages name in
    -- their Cabal files.
    linkArguments :: ![String],
    -- | The arguments of a link whose inputs are bitcode: the LTO argument
    -- and the level of the build. Empty for every other link.
    linkLtoArguments :: ![String]
  }

-- | Link the objects and archives into the executable. The runtime units
-- and the entry are among the objects: the C and Lir objects of every
-- package are linked as they are, so nothing of the runtime is left to a
-- member search. A program with an input from @cxx-sources@ also links
-- the C++ standard library of the target. The system libraries of the
-- packages follow the archives.
linkExecutable :: NativeTarget -> FilePath -> LinkLibraries -> [FilePath] -> [FilePath] -> IO ()
linkExecutable Wasm32Wasip3 output LinkLibraries {linkCxxStdLib, linkArguments} objects archives =
  withTemporaryDirectory "aihc-wasm-link" $ \directory -> do
    when linkCxxStdLib (either (ioError . userError) (const (pure ())) (cxxStandardLibraryArguments Wasm32Wasip3))
    sysroot <- wasmSysroot
    world <- wasip3WorldPath
    let coreModule = directory </> "program.wasm"
        typedModule = directory </> "program-typed.wasm"
    -- The libc archive follows every other input. A linker takes only the
    -- members that resolve a symbol it has already seen, so this pulls the
    -- allocator, the memory routines, and the math functions the runtime
    -- leaves undefined, and nothing else.
    runTool
      "wasm-ld"
      ( ["--no-entry", "--export-memory", "--allow-undefined"]
          <> objects
          <> archives
          <> linkArguments
          <> [wasmSysrootLibc sysroot, "-o", coreModule]
      )
    -- The component type of the world the runtime implements. wit-bindgen
    -- would put it in an object beside the bindings it generates; the
    -- bindings are committed with the runtime instead, so the type is
    -- embedded here from the same world.
    runTool "wasm-tools" ["component", "embed", world, "--world", "command", coreModule, "-o", typedModule]
    buildComponent typedModule output
    runTool "wasm-tools" ["validate", output]
linkExecutable target output LinkLibraries {linkCxxStdLib, linkArguments, linkLtoArguments} objects archives = do
  (compiler, arguments) <- backendCompiler target
  cxxArguments <- if linkCxxStdLib then either (ioError . userError) pure (cxxStandardLibraryArguments target) else pure []
  -- The runtime takes the functions of the Floating class from libm. Recent
  -- platforms carry it inside libc, and -lm is how the older ones that keep
  -- it apart still resolve them.
  runTool compiler (arguments <> executableLinkArguments target <> linkLtoArguments <> objects <> archives <> linkArguments <> ["-lm"] <> cxxArguments <> ["-o", output])

-- | Encode the linked core module as a component. The component model has no
-- way to describe a WASI preview 1 import, so a runtime unit that reaches a
-- libc function needing one fails here rather than at run time. The notice
-- names that cause, which the encoder reports only as an unresolved import.
buildComponent :: FilePath -> FilePath -> IO ()
buildComponent coreModule output = do
  result <- readProcessWithExitCode "wasm-tools" ["component", "new", coreModule, "-o", output] ""
  case result of
    (ExitSuccess, _, _) -> pure ()
    (exitCode, stdout, stderr) -> do
      let reported = if null stderr then stdout else stderr
          notice
            | "wasi_snapshot_preview1" `isInfixOf` reported =
                reported
                  <> "\n\nAIHC notice: the program imports WASI preview 1. The runtime reaches\n\
                     \WASI through the preview 3 bindings only, so this comes from a libc\n\
                     \function that needs the host, such as one of the stdio, exit, or clock\n\
                     \families. Implement it in the P3 IO backend instead.\n"
            | otherwise = reported
      ioError (userError ("wasm-tools failed (" <> show exitCode <> "): " <> notice))

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  result <-
    if tool == "clang" && any ("--target=wasm32" `isPrefixOf`) arguments
      then readWasmClangProcessWithExitCode tool arguments
      else readProcessWithExitCode tool arguments ""
  case result of
    (ExitSuccess, _, _) -> pure ()
    (exitCode, stdout, stderr) -> ioError (userError (tool <> " failed (" <> show exitCode <> "): " <> if null stderr then stdout else stderr))

withTemporaryDirectory :: String -> (FilePath -> IO value) -> IO value
withTemporaryDirectory template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary template
      hClose handle
      removeFile path
      createDirectory path
      pure path
