{-# LANGUAGE OverloadedStrings #-}

-- | Architecture-neutral support shared by backend code generators.
module Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    NativeTarget (..),
    OptimizationLevel (..),
    WasmSysroot (..),
    backendArchiver,
    backendCompiler,
    cxxStandardLibraryArguments,
    handwrittenCArguments,
    buildAddrLiteralPool,
    defaultOptimizationLevel,
    runtimeOptimizationLevel,
    executableEntryName,
    executableEntryParts,
    hostNativeTarget,
    nativeTargetTriple,
    nativeTargetStoreDirectory,
    nativeCpsPrimitiveCall,
    nativeCpsPrimitiveCalls,
    nativeRuntimePrimitiveCall,
    nativeRuntimePrimitiveCalls,
    optimizationArgument,
    parseNativeTarget,
    parseOptimizationLevel,
    readWasmClangProcessWithExitCode,
    renderLinkedFunctionSymbol,
    renderLinkedConstructorInfoSymbol,
    renderLinkedPartialConstructorInfoSymbol,
    renderLinkedGlobalSymbol,
    renderNativeTarget,
    renderOptimizationLevel,
    wasmClangCommand,
    wasmSysroot,
  )
where

import Aihc.Grin.Syntax
import Control.Monad (filterM)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate, intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import System.Info qualified as System
import System.Process (readProcessWithExitCode)

-- | The fixed linked global that starts each executable.
executableEntryName :: Text
executableEntryName = T.intercalate "\0" [package, moduleName, name]
  where
    (package, moduleName, name) = executableEntryParts

-- | The package, module, and name of the global that starts each
-- executable.
executableEntryParts :: (Text, Text, Text)
executableEntryParts = ("exe", "Aihc.Entry", "entry")

-- | A complete backend and executable target.
-- Every target consumes Lir. See @docs/lir.md@.
data NativeTarget
  = AppleArm64
  | LinuxAmd64
  | Llvm
  | Wasm32Wasip3
  deriving (Bounded, Enum, Eq, Ord, Show)

renderNativeTarget :: NativeTarget -> String
renderNativeTarget target =
  case target of
    AppleArm64 -> "apple-arm64"
    LinuxAmd64 -> "linux-amd64"
    Llvm -> "llvm"
    Wasm32Wasip3 -> "wasm32-wasip3"

parseNativeTarget :: String -> Either String NativeTarget
parseNativeTarget value =
  case value of
    "apple-arm64" -> Right AppleArm64
    "arm64-apple-darwin" -> Right AppleArm64
    "linux-amd64" -> Right LinuxAmd64
    "x86_64-unknown-linux-gnu" -> Right LinuxAmd64
    "llvm" -> Right Llvm
    "wasm32-wasip3" -> Right Wasm32Wasip3
    "wasip3" -> Right Wasm32Wasip3
    _ -> Left "target must be apple-arm64, linux-amd64, llvm, or wasm32-wasip3"

-- | Render a NUL-separated logical linker identity as a readable, reversible
-- object symbol. ASCII letters and digits stay intact, components use a single
-- underscore separator, and only literal underscores or unsafe UTF-8 bytes
-- are escaped.
--
-- An escape is @__@, an optional decimal repeat count, and then either a
-- single-letter code from 'escapeCodes' or @x@ and two hex digits. Reading it
-- back is unambiguous: the count stops at the first non-digit, which is the
-- code. Both the count and the codes exist because tuple constructors are
-- almost entirely punctuation, and spelling @(,,,)@ out one @__x2c@ at a time
-- made the symbol grow with the arity and the object file grow with its cube.
--
-- Reading it back needs the components to be non-empty, which
-- 'Aihc.Grin.Syntax.grinScopedName' guarantees by always supplying a package,
-- a module, and a base name. An empty one would put two separators in a row,
-- and a run of underscores cannot say how it was split.
renderLinkedFunctionSymbol :: Text -> Text
renderLinkedFunctionSymbol logicalName =
  Text.decodeUtf8 (BL.toStrict (Builder.toLazyByteString rendered))
  where
    rendered =
      case BS.split 0 (Text.encodeUtf8 logicalName) of
        [unstructured] -> Builder.string7 "aihc_entry_" <> renderComponent unstructured
        components -> mconcat (intersperse (Builder.word8 underscore) (map renderComponent components))
    -- Copy the run of bytes that stay intact, then escape the run of the byte
    -- that stops it. Almost every name is one such intact run.
    renderComponent bytes =
      case BS.span asciiAlphaNumeric bytes of
        (intact, rest) ->
          Builder.byteString intact <> case BS.uncons rest of
            Nothing -> mempty
            Just (byte, remaining) ->
              case BS.span (== byte) remaining of
                (repeated, following) ->
                  renderRun byte (1 + BS.length repeated) <> renderComponent following
    -- A count of one stays implicit so that every name has one spelling.
    renderRun byte count =
      Builder.string7 "__"
        <> (if count == 1 then mempty else Builder.string7 (show count))
        <> renderCode byte
    renderCode byte =
      case lookup byte escapeCodes of
        Just code -> Builder.word8 code
        Nothing ->
          Builder.word8 lowerX
            <> Builder.word8 (hexDigit (byte `shiftR` 4))
            <> Builder.word8 (hexDigit (byte .&. 0x0f))
    hexDigit nibble
      | nibble < 10 = 48 + nibble
      | otherwise = 87 + nibble
    asciiAlphaNumeric byte =
      (byte >= 48 && byte <= 57)
        || (byte >= 65 && byte <= 90)
        || (byte >= 97 && byte <= 122)
    underscore = 95
    lowerX = 120

-- | Short escape codes for the bytes that Haskell names use most, measured
-- over the installed store. A coded escape costs three bytes where the hex
-- form costs five.
--
-- A code may be neither @x@, which introduces the hex form, nor a digit,
-- which a repeat count uses. Any byte without a code still round-trips
-- through the hex form, so this table is a size choice and not a limit on
-- what can be named.
escapeCodes :: [(Word8, Word8)]
escapeCodes = [(ascii source, ascii code) | (source, code) <- table]
  where
    ascii = fromIntegral . fromEnum
    table =
      [ ('_', 'u'),
        (',', 'c'),
        ('.', 'd'),
        ('-', 'm'),
        ('(', 'p'),
        (')', 'q'),
        ('$', 's'),
        ('#', 'h'),
        ('\'', 'r'),
        ('>', 'g'),
        ('=', 'e'),
        ('<', 'l'),
        ('*', 'a'),
        (':', 'o'),
        ('/', 'f'),
        ('+', 't'),
        ('[', 'k'),
        (']', 'j')
      ]

-- | Render the object symbol for one static Haskell value.
renderLinkedGlobalSymbol :: Text -> Text
renderLinkedGlobalSymbol = renderLinkedFunctionSymbol

-- | Render the object symbol for the saturated form of one constructor.
renderLinkedConstructorInfoSymbol :: Text -> Int -> Text
renderLinkedConstructorInfoSymbol name remaining =
  "aihc_c_" <> renderLinkedFunctionSymbol name <> "_" <> T.pack (show remaining)

-- | Render the object symbol for the unsaturated form of one constructor.
-- Every stage between the bare constructor and the saturated one shares this
-- info table and records its own width in the object, so one constructor
-- needs one such symbol however many arguments it takes.
renderLinkedPartialConstructorInfoSymbol :: Text -> Text
renderLinkedPartialConstructorInfoSymbol name =
  "aihc_constructor_" <> renderLinkedFunctionSymbol name <> "_partial"

hostNativeTarget :: Maybe NativeTarget
hostNativeTarget
  | System.os == "darwin" && System.arch `elem` ["aarch64", "arm64"] = Just AppleArm64
  | System.os == "linux" && System.arch == "x86_64" = Just LinuxAmd64
  | otherwise = Nothing

-- | The Clang triple of one target. It selects the C ABI and the libc the
-- objects are compiled against, which is not the same question as the
-- interface a finished program speaks.
--
-- The two differ on WebAssembly, where the triple is one version behind the
-- target name. Preview 3 is not a property of the compilation: the objects
-- are ordinary wasm32 code, and the preview 3 interface comes from the WIT
-- bindings and from the component "wasm-tools" encodes around the linked
-- module. Clang has no preview 3 triple to offer either, and no use for one.
-- What the triple does decide is which libc the runtime agrees with, and the
-- wasi-libc it links was built as @wasm32-wasip1@.
nativeTargetTriple :: NativeTarget -> String
nativeTargetTriple target =
  case target of
    AppleArm64 -> "arm64-apple-darwin"
    LinuxAmd64 -> "x86_64-unknown-linux-gnu"
    Llvm -> "llvm"
    Wasm32Wasip3 -> "wasm32-wasip1"

-- | Render the stable store directory for one compilation target.
nativeTargetStoreDirectory :: NativeTarget -> FilePath
nativeTargetStoreDirectory target =
  case target of
    AppleArm64 -> "arm64-macos-apple"
    LinuxAmd64 -> "amd64-linux-gnu"
    Llvm -> "llvm"
    Wasm32Wasip3 -> "wasm32-wasip3"

-- | The @-O@ level of a build. The level names what the build is for,
-- and "Aihc.Cli.OptimizationPlan" expands it into the scope of the build
-- and the System FC passes to run; no pass reads the level. It is also
-- the level Clang receives for the C sources of a package and for the
-- LLVM output of the @llvm@ target. The object backends do not read it.
data OptimizationLevel
  = O0
  | O1
  | O2
  | Os
  deriving (Eq, Ord, Show, Enum, Bounded)

defaultOptimizationLevel :: OptimizationLevel
defaultOptimizationLevel = O0

-- | The level the runtime and entry archives are compiled at. They are
-- compiled once per target, before any program names a level, and they
-- stay hot for the whole life of every program linked against them.
runtimeOptimizationLevel :: OptimizationLevel
runtimeOptimizationLevel = O2

-- | Parse the argument of a @-O@ option.
parseOptimizationLevel :: String -> Either String OptimizationLevel
parseOptimizationLevel value =
  case value of
    "0" -> Right O0
    "1" -> Right O1
    "2" -> Right O2
    "s" -> Right Os
    _ -> Left "expected 0, 1, 2 or s"

-- | The argument of a level, as the @-O@ option takes it.
renderOptimizationLevel :: OptimizationLevel -> String
renderOptimizationLevel level =
  case level of
    O0 -> "0"
    O1 -> "1"
    O2 -> "2"
    Os -> "s"

-- | The Clang argument of a level.
optimizationArgument :: OptimizationLevel -> String
optimizationArgument level = "-O" <> renderOptimizationLevel level

-- | Arguments for compiling handwritten C: the runtime sources and the C
-- sources of a Hackage package, as opposed to the code aihc generates.
--
-- The runtime takes 'runtimeOptimizationLevel'. It is compiled once per
-- backend and collector, before any program names a level, and it stays hot
-- for the whole life of every program linked against it. An optimizing
-- level is also required rather than merely wanted there: the runtime
-- builds with -Werror, and glibc's features.h raises #warning when
-- _FORTIFY_SOURCE is set without -O, which the Nixpkgs Clang wrapper does.
-- The C sources of a package take the level of the build.
--
-- Callers append their own arguments, so a caller that wants a different level
-- can still override this by passing one later on the command line.
handwrittenCArguments :: OptimizationLevel -> [String]
handwrittenCArguments level = [optimizationArgument level]

-- | Select the compiler driver and target arguments.
backendCompiler :: NativeTarget -> IO (FilePath, [String])
backendCompiler target =
  case target of
    -- No -O flag, matching the other targets. These arguments compile the code
    -- aihc generates, which aihc has already optimised before emitting it, so
    -- Clang's optimiser was redundant work on the hottest path in the compiler:
    -- containers spent 32.7 s of a 39.8 s install in Clang at -O2, against
    -- 8.1 s through the in-house arm64 backend. Handwritten C is a separate
    -- case; see handwrittenCArguments.
    Llvm -> pure ("clang", ["-Wno-override-module"])
    Wasm32Wasip3 -> do
      compiler <- fromMaybe "clang" <$> lookupEnv "AIHC_WASM_CLANG"
      pure
        ( compiler,
          [ "--target=" <> nativeTargetTriple target,
            "-mtail-call",
            "-mmultivalue",
            "-mreference-types",
            "-msign-ext"
          ]
        )
    AppleArm64 -> do
      -- A Linux host compiles for macOS with the SDK headers named by
      -- AIHC_APPLE_SDK and, because the nixpkgs Clang wrapper injects Linux
      -- arguments, usually an unwrapped Clang named by AIHC_APPLE_CLANG.
      compiler <- fromMaybe "clang" <$> lookupEnv "AIHC_APPLE_CLANG"
      sdk <- lookupEnv "AIHC_APPLE_SDK"
      pure (compiler, ["--target=" <> nativeTargetTriple target] <> maybe [] (\root -> ["-isysroot", root]) sdk)
    LinuxAmd64 -> nativeCompiler
  where
    nativeCompiler = pure ("clang", ["--target=" <> nativeTargetTriple target])

-- | The link arguments that add the C++ standard library of a target, for a
-- program that links a package with @cxx-sources@. The C driver links only
-- libc, so the library is named the way GHC's @system-cxx-std-lib@ does it:
-- @libc++@ on macOS and @libstdc++@ on Linux. The @llvm@ target compiles
-- for the host, so the host decides.
--
-- The WASI sysroot holds a libc and no C++ library, so the WebAssembly
-- target has no answer; see 'Aihc.Hackage.Cabal.targetFlagOverrides' for
-- how a package avoids needing one there.
cxxStandardLibraryArguments :: NativeTarget -> Either String [String]
cxxStandardLibraryArguments target =
  case target of
    AppleArm64 -> Right ["-lc++"]
    LinuxAmd64 -> Right ["-lstdc++"]
    Llvm
      | System.os == "darwin" -> Right ["-lc++"]
      | otherwise -> Right ["-lstdc++"]
    Wasm32Wasip3 -> Left "the wasm32-wasip3 target has no C++ standard library: its sysroot supplies libc only, so a package with cxx-sources cannot be linked"

-- | The WASI sysroot that supplies libc to the WebAssembly target. The
-- runtime allocates, copies memory, and aborts through libc like every other
-- target, so a sysroot is required rather than optional.
--
-- The header and archive directories are recorded separately. Their names
-- follow the triple the sysroot was built for, wasi-libc renamed that from
-- @wasm32-wasi@ to @wasm32-wasip1@, and an installation can carry one name
-- for its headers and the other for its archives. Reading both from the
-- directory itself keeps every installation usable without a version test.
data WasmSysroot = WasmSysroot
  { wasmSysrootInclude :: !FilePath,
    wasmSysrootLibc :: !FilePath
  }
  deriving (Eq, Show)

-- | Locate the WASI sysroot. @AIHC_WASM_SYSROOT@ names one directly.
-- Otherwise the well-known installation prefixes are searched, so an
-- ordinary Homebrew or wasi-sdk installation needs no configuration.
wasmSysroot :: IO WasmSysroot
wasmSysroot = do
  override <- lookupEnv "AIHC_WASM_SYSROOT"
  case override of
    Just root -> do
      found <- readWasmSysroot root
      maybe (ioError (userError (missingWasmSysrootMessage (Just root)))) pure found
    Nothing -> do
      found <- traverse readWasmSysroot wasmSysrootCandidates
      case catMaybes found of
        sysroot : _ -> pure sysroot
        [] -> ioError (userError (missingWasmSysrootMessage Nothing))

-- | Read one candidate directory, which is a sysroot when it holds both the
-- headers and the libc archive of a supported target directory.
readWasmSysroot :: FilePath -> IO (Maybe WasmSysroot)
readWasmSysroot root = do
  includes <- filterM (\directory -> doesFileExist (directory </> "stdlib.h")) [root </> "include" </> name | name <- wasmSysrootTargetNames]
  archives <- filterM doesFileExist [root </> "lib" </> name </> "libc.a" | name <- wasmSysrootTargetNames]
  pure $ case (includes, archives) of
    (include : _, archive : _) -> Just WasmSysroot {wasmSysrootInclude = include, wasmSysrootLibc = archive}
    _ -> Nothing

-- | The target directory names wasi-libc has used, newest first.
wasmSysrootTargetNames :: [FilePath]
wasmSysrootTargetNames = ["wasm32-wasip1", "wasm32-wasi"]

-- | The installation prefixes searched when the environment names none.
wasmSysrootCandidates :: [FilePath]
wasmSysrootCandidates =
  [ "/opt/homebrew/opt/wasi-libc/share/wasi-sysroot",
    "/usr/local/opt/wasi-libc/share/wasi-sysroot",
    "/home/linuxbrew/.linuxbrew/opt/wasi-libc/share/wasi-sysroot",
    "/opt/wasi-sdk/share/wasi-sysroot",
    "/usr/local/share/wasi-sysroot",
    "/usr/share/wasi-sysroot"
  ]

missingWasmSysrootMessage :: Maybe FilePath -> String
missingWasmSysrootMessage rejected =
  unlines
    ( introduction
        <> [ "",
             "Install one and, when it is outside a standard prefix, set",
             "AIHC_WASM_SYSROOT to the directory holding include/<target> and",
             "lib/<target>/libc.a:",
             "",
             "  brew install wasi-libc",
             "  https://github.com/WebAssembly/wasi-sdk/releases",
             "",
             "The searched prefixes are:"
           ]
        <> ["  " <> candidate | candidate <- wasmSysrootCandidates]
    )
  where
    introduction =
      case rejected of
        Just root ->
          [ "AIHC_WASM_SYSROOT does not name a WASI sysroot: " <> root,
            "It holds no " <> intercalate " or " [name <> "/libc.a" | name <- wasmSysrootTargetNames] <> " under lib."
          ]
        Nothing -> ["The wasm32-wasip3 target requires a WASI sysroot and none was found."]

-- | Select an archive tool that keeps object files for the selected target.
backendArchiver :: NativeTarget -> IO FilePath
backendArchiver target = do
  override <- lookupEnv "AIHC_LLVM_AR"
  case override of
    Just archiver -> pure archiver
    Nothing -> do
      llvmArchiver <- findExecutable "llvm-ar"
      case llvmArchiver of
        Just archiver -> pure archiver
        Nothing -> do
          archiver <- fromMaybe "ar" <$> findExecutable "ar"
          if System.os == "darwin" && target `elem` [LinuxAmd64, Wasm32Wasip3] && archiver == "/usr/bin/ar"
            then ioError (userError "The selected target requires LLVM ar. Set AIHC_LLVM_AR to its path.")
            else pure archiver

-- | Deduplicate address literals and assign short, unit-local assembly labels.
buildAddrLiteralPool :: GrinProgram -> [(ByteString, Text)]
buildAddrLiteralPool program =
  [ (value, ".Laihc_addr_" <> T.pack (show index))
  | (index, value) <- zip [0 :: Int ..] values
  ]
  where
    values = Set.toAscList (Set.fromList [value | GrinLitAddr value <- grinProgramLiterals program])

-- | Select the ordinary Clang driver used for WebAssembly objects. Nix can
-- override only the executable to bypass its host-target compiler wrapper.
-- The sysroot is not part of this: an assembly input needs no headers, and
-- the C compilations add it themselves.
wasmClangCommand :: Maybe FilePath -> (FilePath, [String])
wasmClangCommand override =
  (fromMaybe "clang" override, ["--target=" <> nativeTargetTriple Wasm32Wasip3])

-- | Run Clang and, after a WebAssembly compilation failure, inspect its
-- registered targets so a target-limited installation gets an actionable
-- diagnostic without obscuring Clang's original error.
readWasmClangProcessWithExitCode :: FilePath -> [String] -> IO (ExitCode, String, String)
readWasmClangProcessWithExitCode clang arguments = do
  result@(exitCode, stdout, stderr) <- readProcessWithExitCode clang arguments ""
  case exitCode of
    ExitSuccess -> pure result
    ExitFailure _ -> do
      targetsResult <- tryIOError (readProcessWithExitCode clang ["-print-targets"] "")
      pure
        ( exitCode,
          stdout,
          case targetsResult of
            Right (ExitSuccess, targets, _targetsStderr)
              | not (hasWasm32Target targets) -> appendWasm32TargetNotice stderr
            _ -> stderr
        )

hasWasm32Target :: String -> Bool
hasWasm32Target = any lineIsWasm32Target . lines
  where
    lineIsWasm32Target line =
      case words line of
        target : _ -> target == "wasm32"
        [] -> False

appendWasm32TargetNotice :: String -> String
appendWasm32TargetNotice originalError =
  originalError
    <> separator
    <> unlines
      [ "AIHC notice: this Clang installation does not include the wasm32 target.",
        "The default Clang shipped with macOS omits WebAssembly support. Install LLVM Clang",
        "with Homebrew (`brew install llvm`) or Nix",
        "(`nix shell nixpkgs#llvmPackages.clang-unwrapped`), then set AIHC_WASM_CLANG",
        "to that Clang executable."
      ]
  where
    separator
      | null originalError = ""
      | last originalError == '\n' = "\n"
      | otherwise = "\n\n"

-- | Control transfer performed after a native CPS runtime call returns.
data NativeCpsTransfer
  = NativeCpsEnterContinuation
  | NativeCpsResumeScheduler
  deriving (Eq, Show)

-- | Architecture-neutral native ABI description for a CPS primitive.
data NativeCpsCall = NativeCpsCall
  { nativeCpsCallSymbol :: !Text,
    nativeCpsCallOperandCount :: !Int,
    nativeCpsCallPassContinuation :: !Bool,
    nativeCpsCallTransfer :: !NativeCpsTransfer
  }
  deriving (Eq, Show)

-- | Architecture-neutral native ABI description for a direct runtime
-- primitive. The machine is an implicit runtime argument rather than a GRIN
-- operand, and the result count describes the logical GRIN result independently
-- of the C function's return type.
data NativeRuntimeCall = NativeRuntimeCall
  { nativeRuntimeCallForeignCall :: !GrinForeignCall,
    nativeRuntimeCallPassMachine :: !Bool,
    nativeRuntimeCallResultCount :: !Int
  }
  deriving (Eq, Show)

nativeCpsPrimitiveCall :: Text -> Maybe NativeCpsCall
nativeCpsPrimitiveCall name = lookup name nativeCpsPrimitiveCalls

nativeCpsPrimitiveCalls :: [(Text, NativeCpsCall)]
nativeCpsPrimitiveCalls =
  [ enters "fork#" "aihc_fork" 1,
    enters "newMVar#" "aihc_mvar_new" 0,
    resumes "readMVar#" "aihc_mvar_read" 1,
    resumes "takeMVar#" "aihc_mvar_take" 1,
    resumes "putMVar#" "aihc_mvar_put" 2,
    resumes "yield#" "aihc_yield" 0,
    resumes "awaitIO#" "aihc_await_io" 1,
    resumes "aihcControl0#" "aihc_control0" 2,
    resumes "aihcResume#" "aihc_continuation_resume" 2
  ]
  where
    enters primitive symbol operands =
      (primitive, NativeCpsCall symbol operands False NativeCpsEnterContinuation)
    resumes primitive symbol operands =
      (primitive, NativeCpsCall symbol operands True NativeCpsResumeScheduler)

-- | Runtime calls shared by native backends. Representation-preserving
-- primitives such as freeze and thaw deliberately have no entry here.
nativeRuntimePrimitiveCall :: Text -> Maybe NativeRuntimeCall
nativeRuntimePrimitiveCall name = lookup name nativeRuntimePrimitiveCalls

nativeRuntimePrimitiveCalls :: [(Text, NativeRuntimeCall)]
nativeRuntimePrimitiveCalls =
  [ machineCall "newArray#" "aihc_array_new" [GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "newTVar#" "aihc_mutvar_new" [GrinForeignWord64] GrinForeignAddr,
    machineCall "readTVar#" "aihc_tvar_read" [GrinForeignAddr] GrinForeignWord64,
    machineCall "readTVarIO#" "aihc_tvar_read" [GrinForeignAddr] GrinForeignWord64,
    runtimeCall True 0 "writeTVar#" "aihc_tvar_write" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    runtimeCall True 0 "stmBegin#" "aihc_stm_begin" [] GrinForeignWord64,
    runtimeCall True 0 "stmCommit#" "aihc_stm_commit" [] GrinForeignWord64,
    runtimeCall True 0 "stmAbort#" "aihc_stm_abort" [] GrinForeignWord64,
    machineCall "submitIORead#" "aihc_io_submit_read" [GrinForeignAddr, GrinForeignAddr, GrinForeignInt64, GrinForeignInt64] GrinForeignAddr,
    machineCall "submitIOWrite#" "aihc_io_submit_write" [GrinForeignAddr, GrinForeignAddr, GrinForeignInt64, GrinForeignInt64] GrinForeignAddr,
    machineCall "submitIOOpen#" "aihc_io_submit_open" [GrinForeignAddr, GrinForeignInt64, GrinForeignInt64] GrinForeignAddr,
    machineCall "stmWaitRequest#" "aihc_stm_wait_request" [] GrinForeignAddr,
    machineCall "stmWaitResult#" "aihc_stm_wait_result" [GrinForeignAddr] GrinForeignInt64,
    machineCall "newDelayTVar#" "aihc_tvar_delay" [GrinForeignInt64, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "stmActive#" "aihc_stm_active" [] GrinForeignWord64,
    machineCall "newMutVar#" "aihc_mutvar_new" [GrinForeignWord64] GrinForeignAddr,
    machineCall "makeStableName#" "aihc_stable_name_make" [GrinForeignAddr] GrinForeignAddr,
    machineCall "newPromptTag#" "aihc_prompt_tag_new" [] GrinForeignAddr,
    -- The thread that runs now is a field of the machine, so this call takes
    -- the machine. It only reads that field, and it allocates nothing.
    machineCall "myThreadId#" "aihc_my_thread_id" [] GrinForeignAddr,
    procedure "copyArray#" "aihc_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyMutableArray#" "aihc_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    machineCall "cloneArray#" "aihc_array_clone" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "cloneMutableArray#" "aihc_array_clone" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "freezeArray#" "aihc_array_clone" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "thawArray#" "aihc_array_clone" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    -- The small-array family shares the boxed-array representation, so every
    -- entry below names the boxed-array runtime function of the same shape.
    machineCall "newSmallArray#" "aihc_array_new" [GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    procedure "copySmallArray#" "aihc_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copySmallMutableArray#" "aihc_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    machineCall "cloneSmallArray#" "aihc_array_clone" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "cloneSmallMutableArray#" "aihc_array_clone" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "freezeSmallArray#" "aihc_array_clone" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    machineCall "thawSmallArray#" "aihc_array_clone" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    procedure "shrinkSmallMutableArray#" "aihc_array_shrink" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    machineCall "resizeSmallMutableArray#" "aihc_array_resize" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    -- The MVar operations that never block, so they are runtime calls rather
    -- than the CPS calls that takeMVar# and putMVar# need. tryTakeMVar# gives
    -- a flag and the contents; the runtime function returns the flag, and
    -- the lowering reads the contents before the operation runs.
    machineCall "tryTakeMVar#" "aihc_mvar_try_take" [GrinForeignAddr] GrinForeignWord64,
    machineCall "tryPutMVar#" "aihc_mvar_try_put" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "aihcByteArrayWords#" "aihc_byte_array_words" [GrinForeignWord64, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "aihcResizeByteArrayWords#" "aihc_byte_array_resize_words" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    machineCall "newByteArray#" "aihc_byte_array_new" [GrinForeignWord64] GrinForeignAddr,
    machineCall "newPinnedByteArray#" "aihc_byte_array_new_pinned" [GrinForeignWord64] GrinForeignAddr,
    machineCall "newAlignedPinnedByteArray#" "aihc_byte_array_new_aligned_pinned" [GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    procedure "shrinkMutableByteArray#" "aihc_byte_array_shrink" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    machineCall "resizeMutableByteArray#" "aihc_byte_array_resize" [GrinForeignAddr, GrinForeignWord64] GrinForeignAddr,
    procedure "copyAddrToByteArray#" "aihc_byte_array_copy_from_addr" [GrinForeignAddr, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "fetchAddIntArray#" "aihc_byte_array_fetch_add_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "fetchSubIntArray#" "aihc_byte_array_fetch_sub_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "fetchAndIntArray#" "aihc_byte_array_fetch_and_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "fetchNandIntArray#" "aihc_byte_array_fetch_nand_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "fetchOrIntArray#" "aihc_byte_array_fetch_or_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "fetchXorIntArray#" "aihc_byte_array_fetch_xor_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "casIntArray#" "aihc_byte_array_compare_and_swap_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyByteArray#" "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyMutableByteArray#" "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyMutableByteArrayNonOverlapping#" "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyByteArrayToAddr#" "aihc_byte_array_copy_to_addr" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "copyMutableByteArrayToAddr#" "aihc_byte_array_copy_to_addr" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "compareByteArrays#" "aihc_byte_array_compare" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "expDouble#" "exp" [GrinForeignDouble] GrinForeignDouble,
    call "logDouble#" "log" [GrinForeignDouble] GrinForeignDouble,
    call "sinDouble#" "sin" [GrinForeignDouble] GrinForeignDouble,
    call "cosDouble#" "cos" [GrinForeignDouble] GrinForeignDouble,
    call "tanDouble#" "tan" [GrinForeignDouble] GrinForeignDouble,
    call "asinDouble#" "asin" [GrinForeignDouble] GrinForeignDouble,
    call "acosDouble#" "acos" [GrinForeignDouble] GrinForeignDouble,
    call "atanDouble#" "atan" [GrinForeignDouble] GrinForeignDouble,
    call "sinhDouble#" "sinh" [GrinForeignDouble] GrinForeignDouble,
    call "coshDouble#" "cosh" [GrinForeignDouble] GrinForeignDouble,
    call "tanhDouble#" "tanh" [GrinForeignDouble] GrinForeignDouble,
    call "asinhDouble#" "asinh" [GrinForeignDouble] GrinForeignDouble,
    call "acoshDouble#" "acosh" [GrinForeignDouble] GrinForeignDouble,
    call "atanhDouble#" "atanh" [GrinForeignDouble] GrinForeignDouble,
    call "**##" "pow" [GrinForeignDouble, GrinForeignDouble] GrinForeignDouble,
    call "expFloat#" "expf" [GrinForeignFloat] GrinForeignFloat,
    call "logFloat#" "logf" [GrinForeignFloat] GrinForeignFloat,
    call "sinFloat#" "sinf" [GrinForeignFloat] GrinForeignFloat,
    call "cosFloat#" "cosf" [GrinForeignFloat] GrinForeignFloat,
    call "tanFloat#" "tanf" [GrinForeignFloat] GrinForeignFloat,
    call "asinFloat#" "asinf" [GrinForeignFloat] GrinForeignFloat,
    call "acosFloat#" "acosf" [GrinForeignFloat] GrinForeignFloat,
    call "atanFloat#" "atanf" [GrinForeignFloat] GrinForeignFloat,
    call "sinhFloat#" "sinhf" [GrinForeignFloat] GrinForeignFloat,
    call "coshFloat#" "coshf" [GrinForeignFloat] GrinForeignFloat,
    call "tanhFloat#" "tanhf" [GrinForeignFloat] GrinForeignFloat,
    call "asinhFloat#" "asinhf" [GrinForeignFloat] GrinForeignFloat,
    call "acoshFloat#" "acoshf" [GrinForeignFloat] GrinForeignFloat,
    call "atanhFloat#" "atanhf" [GrinForeignFloat] GrinForeignFloat,
    call "powerFloat#" "powf" [GrinForeignFloat, GrinForeignFloat] GrinForeignFloat,
    procedure "setByteArray#" "aihc_byte_array_set" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64
  ]
  where
    call = runtimeCall False 1
    procedure = runtimeCall False 0
    machineCall = runtimeCall True 1

-- | Describe one runtime call in the shared native ABI.
runtimeCall :: Bool -> Int -> Text -> Text -> [GrinForeignType] -> GrinForeignType -> (Text, NativeRuntimeCall)
runtimeCall passMachine resultCount primitive symbol arguments result =
  ( primitive,
    NativeRuntimeCall
      { nativeRuntimeCallForeignCall =
          GrinForeignCall
            { grinForeignCallName = "$runtime$" <> symbol,
              grinForeignCallSymbol = symbol,
              grinForeignCallTarget = GrinForeignFunction,
              grinForeignCallSignature =
                GrinForeignSignature
                  { grinForeignArgumentTypes = arguments,
                    grinForeignResultType = result,
                    grinForeignEffect = GrinForeignPure
                  }
            },
        nativeRuntimeCallPassMachine = passMachine,
        nativeRuntimeCallResultCount = resultCount
      }
  )
