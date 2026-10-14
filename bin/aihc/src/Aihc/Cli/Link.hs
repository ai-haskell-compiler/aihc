{-# LANGUAGE OverloadedStrings #-}

-- | The final step of @build@: link the objects of an executable and the
-- packages below it, or write them to a link bundle for @link-exe@.
module Aihc.Cli.Link
  ( LinkBundle (..),
    linkBundleManifestPath,
    readLinkBundle,
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
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    makeAbsolute,
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)

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
  compileEntryObject lto (compileProfileAllocations compileConfig) target buildRoot entry
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
        object <- compileLtoProgram (compiledPrimPackage executable) compileConfig buildRoot corePaths
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

-- | The target and Clang arguments for a native link.
-- Input paths are relative to the bundle directory.
data LinkBundle = LinkBundle
  { linkBundleTarget :: !NativeTarget,
    linkBundleArguments :: ![String]
  }
  deriving (Eq, Show)

linkBundleManifestPath :: FilePath -> FilePath
linkBundleManifestPath bundle = bundle </> "link.args"

linkBundleTargetPath :: FilePath -> FilePath
linkBundleTargetPath bundle = bundle </> "link.target"

readLinkBundle :: FilePath -> IO (Either String LinkBundle)
readLinkBundle path = do
  target <- TIO.readFile (linkBundleTargetPath (takeDirectory path))
  arguments <- TIO.readFile path
  pure $ LinkBundle <$> parseNativeTarget (T.unpack (T.strip target)) <*> parseLinkArguments (T.unpack arguments)

-- | Each argument has double quotes. Escape quotes and backslashes.
renderLinkArguments :: [String] -> T.Text
renderLinkArguments = T.pack . unlines . map quote
  where
    quote argument = "\"" <> concatMap escape argument <> "\""
    escape character
      | character `elem` ['\"', '\\'] = ['\\', character]
      | otherwise = [character]

-- | Read the quoted arguments that the bundle writer produces.
parseLinkArguments :: String -> Either String [String]
parseLinkArguments input =
  case dropWhile isSpace input of
    [] -> Right []
    '"' : rest -> do
      (argument, remaining) <- quoted [] rest
      case remaining of
        character : _ | not (isSpace character) -> Left "invalid response file separator"
        _ -> (argument :) <$> parseLinkArguments remaining
    _ -> Left "expected a quoted response file argument"
  where
    quoted result ('"' : rest) = Right (reverse result, rest)
    quoted result ('\\' : character : rest)
      | character `elem` ['"', '\\'] = quoted (character : result) rest
    quoted _ ('\\' : _) = Left "invalid response file escape"
    quoted _ [] = Left "incomplete response file argument"
    quoted result (character : rest) = quoted (character : result) rest

-- | Each bundle has copies of its inputs and a Clang response file.
writeLinkBundle :: NativeTarget -> FilePath -> LinkLibraries -> [FilePath] -> [FilePath] -> IO ()
writeLinkBundle Wasm32Wasip3 _ _ _ _ = ioError (userError "AIHC does not support WASM link bundles")
writeLinkBundle target bundle libraries objects archives = do
  let inputs = bundle </> "inputs"
  createDirectoryIfMissing True inputs
  copied <- forM (zip [0 :: Int ..] (objects <> archives)) $ \(index, source) -> do
    let name = padIndex index <> "-" <> takeFileName source
    copyFile source (inputs </> name)
    pure ("inputs" </> name)
  arguments <- either (ioError . userError) pure (nativeLinkArguments target libraries copied)
  TIO.writeFile (linkBundleManifestPath bundle) (renderLinkArguments arguments)
  TIO.writeFile (linkBundleTargetPath bundle) (T.pack (renderNativeTarget target <> "\n"))
  where
    padIndex index = replicate (4 - length (show index)) '0' <> show index

runLinkExe :: LinkExeOptions -> IO ()
runLinkExe options = do
  bundle <- makeAbsolute (linkExeBundle options)
  output <- makeAbsolute (linkExeOutputFile options)
  let manifest = linkBundleManifestPath bundle
  exists <- doesFileExist manifest
  unless exists (ioError (userError ("No link response file at " <> manifest)))
  decoded <- readLinkBundle manifest
  LinkBundle {linkBundleTarget} <-
    either (ioError . userError . (("Invalid link bundle " <> manifest <> ": ") <>)) pure decoded
  when (linkBundleTarget == Wasm32Wasip3) (ioError (userError "AIHC does not support WASM link bundles"))
  createDirectoryIfMissing True (takeDirectory output)
  (compiler, hostArguments) <- backendCompiler linkBundleTarget
  result <- readCreateProcessWithExitCode (proc compiler (hostArguments <> ["@link.args", "-o", output])) {cwd = Just bundle} ""
  checkToolResult compiler result

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
linkExecutable Wasm32Wasip3 output LinkLibraries {linkCxxStdLib, linkArguments} objects archives = do
  when linkCxxStdLib (either (ioError . userError) (const (pure ())) (cxxStandardLibraryArguments Wasm32Wasip3))
  sysroot <- wasmSysroot
  world <- wasip3WorldPath
  -- wasm-component-ld links the core module with wasm-ld and encodes it as a
  -- component. The libc of the sysroot carries the component type of the
  -- interfaces it imports, and the world of the runtime is added to it. The
  -- libc archive follows every other input: a linker takes only the members
  -- that resolve a symbol it has already seen. The libc keeps its thread-local
  -- storage in a segment, which needs the atomics feature, although the
  -- program has one thread: the runtime supplies the functions that find the
  -- stack pointer and the segment. WASI has no file modes, so the runtime
  -- answers chmod and fchmod itself.
  runTool
    "wasm-component-ld"
    ( ["-m", "wasm32", "--no-entry", "--export-memory", "--component-type", world, "--append-lld-flag=--extra-features=atomics", "--append-lld-flag=--wrap=chmod", "--append-lld-flag=--wrap=fchmod"]
        <> objects
        <> archives
        <> linkArguments
        <> [wasmSysrootLibc sysroot]
        <> maybe [] pure (wasmSysrootBuiltins sysroot)
        <> ["-o", output]
    )
  runTool "wasm-tools" ["validate", output]
linkExecutable target output libraries objects archives = do
  (compiler, hostArguments) <- backendCompiler target
  arguments <- either (ioError . userError) pure (nativeLinkArguments target libraries (objects <> archives))
  runTool compiler (hostArguments <> arguments <> ["-o", output])

-- | Keep input order. System libraries follow all objects and archives.
nativeLinkArguments :: NativeTarget -> LinkLibraries -> [FilePath] -> Either String [String]
nativeLinkArguments target LinkLibraries {linkCxxStdLib, linkArguments, linkLtoArguments} inputs = do
  cxxArguments <- if linkCxxStdLib then cxxStandardLibraryArguments target else pure []
  pure (executableLinkArguments target <> linkLtoArguments <> inputs <> linkArguments <> ["-lm"] <> cxxArguments)

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  result <-
    if tool == "clang" && any ("--target=wasm32" `isPrefixOf`) arguments
      then readWasmClangProcessWithExitCode tool arguments
      else readProcessWithExitCode tool arguments ""
  checkToolResult tool result

checkToolResult :: FilePath -> (ExitCode, String, String) -> IO ()
checkToolResult tool result =
  case result of
    (ExitSuccess, _, _) -> pure ()
    (exitCode, stdout, stderr) -> ioError (userError (tool <> " failed (" <> show exitCode <> "): " <> if null stderr then stdout else stderr))
