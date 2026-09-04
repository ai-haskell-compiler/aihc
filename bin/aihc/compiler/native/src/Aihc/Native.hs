{-# LANGUAGE OverloadedStrings #-}

-- | Architecture-neutral support shared by backend code generators.
module Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    NativeTarget (..),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    backendArchiver,
    backendCompiler,
    buildAddrLiteralPool,
    executableEntryName,
    hostNativeTarget,
    nativeTargetTriple,
    nativeTargetStoreDirectory,
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
    nativeSplitRuntimePrimitiveCall,
    parseNativeTarget,
    renderLinkedFunctionSymbol,
    renderLinkedConstructorInfoSymbol,
    renderLinkedGlobalSymbol,
    renderNativeTarget,
    runtimePlan,
    supportedNativePrimitiveNames,
  )
where

import Aihc.Grin.Syntax
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Numeric (showHex)
import Paths_aihc (getDataFileName)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.Info qualified as System

-- | The fixed linked global that starts each executable.
executableEntryName :: Text
executableEntryName = T.intercalate "\0" ["exe", "Aihc.Entry", "entry"]

-- | A complete backend and executable target.
-- Every target consumes Lir. See @docs/lir.md@.
data NativeTarget
  = AppleArm64
  | LinuxAmd64
  | Llvm
  | Wasm32Wasip3
  deriving (Bounded, Enum, Eq, Ord, Show)

data RuntimeGarbageCollector
  = RuntimeGcSemispace
  deriving (Eq, Ord, Show)

data RuntimePlan = RuntimePlan
  { runtimeSources :: ![FilePath],
    runtimeIncludeDirectories :: ![FilePath]
  }
  deriving (Eq, Show)

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
renderLinkedFunctionSymbol :: Text -> Text
renderLinkedFunctionSymbol logicalName =
  case T.splitOn "\0" logicalName of
    [unstructured] -> "aihc_entry_" <> renderComponent unstructured
    components -> T.intercalate "_" (map renderComponent components)
  where
    renderComponent = T.concat . map renderByte . BS.unpack . Text.encodeUtf8
    renderByte byte
      | asciiAlphaNumeric byte = T.singleton (chr (fromIntegral byte))
      | byte == 95 = "__u"
      | otherwise = "__x" <> T.pack (padByte (showHex byte ""))
    asciiAlphaNumeric byte =
      (byte >= 48 && byte <= 57)
        || (byte >= 65 && byte <= 90)
        || (byte >= 97 && byte <= 122)
    padByte [digit] = ['0', digit]
    padByte digits = digits

-- | Render the object symbol for one static Haskell value.
renderLinkedGlobalSymbol :: Text -> Text
renderLinkedGlobalSymbol = renderLinkedFunctionSymbol

-- | Render the object symbol for one constructor application stage.
renderLinkedConstructorInfoSymbol :: Text -> Int -> Text
renderLinkedConstructorInfoSymbol name remaining =
  "aihc_constructor_" <> renderLinkedFunctionSymbol name <> "_" <> T.pack (show remaining)

hostNativeTarget :: Maybe NativeTarget
hostNativeTarget
  | System.os == "darwin" && System.arch `elem` ["aarch64", "arm64"] = Just AppleArm64
  | System.os == "linux" && System.arch == "x86_64" = Just LinuxAmd64
  | otherwise = Nothing

nativeTargetTriple :: NativeTarget -> String
nativeTargetTriple target =
  case target of
    AppleArm64 -> "arm64-apple-darwin"
    LinuxAmd64 -> "x86_64-unknown-linux-gnu"
    Llvm -> "llvm"
    Wasm32Wasip3 -> "wasm32-unknown-unknown"

-- | Render the stable store directory for one compilation target.
nativeTargetStoreDirectory :: NativeTarget -> FilePath
nativeTargetStoreDirectory target =
  case target of
    AppleArm64 -> "arm64-macos-apple"
    LinuxAmd64 -> "amd64-linux-gnu"
    Llvm -> "llvm"
    Wasm32Wasip3 -> "wasm32-wasip3"

-- | Select the compiler driver and target arguments.
backendCompiler :: NativeTarget -> IO (FilePath, [String])
backendCompiler target =
  case target of
    Llvm -> pure ("clang", ["-Wno-override-module", "-O2"])
    Wasm32Wasip3 -> do
      compiler <- fromMaybe "clang" <$> lookupEnv "AIHC_WASM_CLANG"
      pure (compiler, ["--target=wasm32-unknown-unknown", "-mtail-call", "-mmultivalue", "-mreference-types", "-msign-ext"])
    AppleArm64 -> nativeCompiler
    LinuxAmd64 -> nativeCompiler
  where
    nativeCompiler = pure ("clang", ["--target=" <> nativeTargetTriple target])

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

runtimeSourcePath :: IO FilePath
runtimeSourcePath = getDataFileName "compiler/native/runtime/aihc_runtime.c"

runtimePlan :: NativeTarget -> RuntimeGarbageCollector -> IO RuntimePlan
runtimePlan target garbageCollector = do
  core <- runtimeSourcePath
  runtimeOptions <- getDataFileName "compiler/native/runtime/aihc_runtime_options.c"
  collector <-
    getDataFileName $ case garbageCollector of
      RuntimeGcSemispace -> "compiler/native/runtime/aihc_gc_semispace.c"
  host <-
    getDataFileName $ case target of
      Wasm32Wasip3 -> "compiler/native/runtime/aihc_host_wasip3.c"
      _ -> "compiler/native/runtime/aihc_host_posix.c"
  pure
    RuntimePlan
      { runtimeSources = [core, runtimeOptions, collector, host],
        runtimeIncludeDirectories = [takeDirectory core]
      }

-- | Primitive operations implemented directly by every native backend or by
-- the shared runtime ABI.
supportedNativePrimitiveNames :: [Text]
supportedNativePrimitiveNames =
  [ "+#",
    "-#",
    "*#",
    "compareInt#",
    "<#",
    "==#",
    "ord#",
    "chr#",
    "addIntC#",
    "subIntC#",
    "plusWord#",
    "minusWord#",
    "timesWord#",
    "addWordC#",
    "subWordC#",
    "timesWord2#",
    "quotWord#",
    "remWord#",
    "quotRemWord#",
    "quotRemWord2#",
    "and#",
    "or#",
    "xor#",
    "not#",
    "uncheckedShiftL#",
    "uncheckedShiftRL#",
    "int2Word#",
    "word2Int#",
    "word8ToWord#",
    "word32ToWord#",
    "word64ToWord#",
    "wordToWord64#",
    "word16ToWord#",
    ">#",
    ">=#",
    "<=#",
    "/=#",
    "eqWord64#",
    "neWord64#",
    "ltWord64#",
    "leWord64#",
    "gtWord64#",
    "geWord64#",
    "eqWord#",
    "neWord#",
    "ltWord#",
    "leWord#",
    "gtWord#",
    "geWord#",
    "nullAddr#",
    "realWorld#",
    "unsafeFreezeArray#",
    "unsafeThawArray#",
    "unsafeFreezeByteArray#",
    "unsafeThawByteArray#",
    "castFloatToWord32#",
    "castWord32ToFloat#",
    "castDoubleToWord64#",
    "castWord64ToDouble#"
  ]
    <> map fst nativeCpsPrimitiveCalls
    <> map fst nativeRuntimePrimitiveCalls
    <> map fst nativeSplitRuntimePrimitiveCalls

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
    resumes "awaitIO#" "aihc_await_io" 1
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
    machineCall "newMutVar#" "aihc_mutvar_new" [GrinForeignWord64] GrinForeignAddr,
    machineCall "makeStableName#" "aihc_stable_name_make" [GrinForeignAddr] GrinForeignAddr,
    call "readMutVar#" "aihc_mutvar_read" [GrinForeignAddr] GrinForeignWord64,
    procedure "writeMutVar#" "aihc_mutvar_write" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    pairCall "casMutVar#" "aihc_mutvar_compare_and_swap" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "sameMutVar#" "aihc_mutvar_same" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "eqStableName#" "aihc_stable_name_equal" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "stableNameToInt#" "aihc_stable_name_hash" [GrinForeignAddr] GrinForeignWord64,
    call "indexWord8OffAddr#" "aihc_addr_index_word8" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord32OffAddr#" "aihc_addr_index_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord64OffAddr#" "aihc_addr_index_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord16OffAddr#" "aihc_addr_index_word16" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord8OffAddr#" "aihc_addr_index_word8" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord16OffAddr#" "aihc_addr_index_word16" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord32OffAddr#" "aihc_addr_index_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord64OffAddr#" "aihc_addr_index_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord8OffAddr#" "aihc_addr_write_word8" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord16OffAddr#" "aihc_addr_write_word16" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord32OffAddr#" "aihc_addr_write_word32" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord64OffAddr#" "aihc_addr_write_word64" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8OffAddrAsWord16#" "aihc_addr_index_byte_word16" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8OffAddrAsWord32#" "aihc_addr_index_byte_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8OffAddrAsWord64#" "aihc_addr_index_byte_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord8OffAddrAsWord16#" "aihc_addr_index_byte_word16" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord8OffAddrAsWord32#" "aihc_addr_index_byte_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord8OffAddrAsWord64#" "aihc_addr_index_byte_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord8OffAddrAsWord16#" "aihc_addr_write_byte_word16" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord8OffAddrAsWord32#" "aihc_addr_write_byte_word32" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord8OffAddrAsWord64#" "aihc_addr_write_byte_word64" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "plusAddr#" "aihc_addr_plus" [GrinForeignAddr, GrinForeignWord64] GrinForeignAddr,
    call "minusAddr#" "aihc_addr_minus" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "eqAddr#" "aihc_addr_eq" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "neAddr#" "aihc_addr_ne" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "ltAddr#" "aihc_addr_lt" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "leAddr#" "aihc_addr_le" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "gtAddr#" "aihc_addr_gt" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "geAddr#" "aihc_addr_ge" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "addr2Int#" "aihc_addr_to_int" [GrinForeignAddr] GrinForeignWord64,
    call "int2Addr#" "aihc_int_to_addr" [GrinForeignWord64] GrinForeignAddr,
    call "cstringLength#" "aihc_addr_cstring_length" [GrinForeignAddr] GrinForeignWord64,
    procedure "touch#" "aihc_touch" [GrinForeignWord64] GrinForeignWord64,
    call "wordToWord8#" "aihc_word_to_word8" [GrinForeignWord64] GrinForeignWord64,
    call "wordToWord16#" "aihc_word_to_word16" [GrinForeignWord64] GrinForeignWord64,
    call "wordToWord32#" "aihc_word_to_word32" [GrinForeignWord64] GrinForeignWord64,
    call "indexArray#" "aihc_array_index" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readArray#" "aihc_array_index" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "writeArray#" "aihc_array_write" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "sameMutableArray#" "aihc_array_same" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "newByteArray#" "aihc_byte_array_new" [GrinForeignWord64] GrinForeignAddr,
    call "newPinnedByteArray#" "aihc_byte_array_new_pinned" [GrinForeignWord64] GrinForeignAddr,
    call "newAlignedPinnedByteArray#" "aihc_byte_array_new_aligned_pinned" [GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    call "isMutableByteArrayPinned#" "aihc_byte_array_is_pinned" [GrinForeignAddr] GrinForeignWord64,
    call "isByteArrayPinned#" "aihc_byte_array_is_pinned" [GrinForeignAddr] GrinForeignWord64,
    call "byteArrayContents#" "aihc_byte_array_contents" [GrinForeignAddr] GrinForeignAddr,
    call "mutableByteArrayContents#" "aihc_byte_array_contents" [GrinForeignAddr] GrinForeignAddr,
    procedure "shrinkMutableByteArray#" "aihc_byte_array_shrink" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "resizeMutableByteArray#" "aihc_byte_array_resize" [GrinForeignAddr, GrinForeignWord64] GrinForeignAddr,
    call "sizeofByteArray#" "aihc_byte_array_get_size" [GrinForeignAddr] GrinForeignWord64,
    call "getSizeofMutableByteArray#" "aihc_byte_array_get_size" [GrinForeignAddr] GrinForeignWord64,
    procedure "copyAddrToByteArray#" "aihc_byte_array_copy_from_addr" [GrinForeignAddr, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "indexWordArray#" "aihc_byte_array_index_word" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWordArray#" "aihc_byte_array_read_word" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWordArray#" "aihc_byte_array_write_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyByteArray#" "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyMutableByteArray#" "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "copyByteArrayToAddr#" "aihc_byte_array_copy_to_addr" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "copyMutableByteArrayToAddr#" "aihc_byte_array_copy_to_addr" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "compareByteArrays#" "aihc_byte_array_compare" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "clz#" "aihc_word_clz" [GrinForeignWord64] GrinForeignWord64,
    call "ctz#" "aihc_word_ctz" [GrinForeignWord64] GrinForeignWord64,
    call "popCnt#" "aihc_word_popcount" [GrinForeignWord64] GrinForeignWord64,
    call "intToInt8#" "aihc_int_to_int8" [GrinForeignWord64] GrinForeignWord64,
    call "int8ToInt#" "aihc_int8_to_int" [GrinForeignWord64] GrinForeignWord64,
    call "intToInt16#" "aihc_int_to_int16" [GrinForeignWord64] GrinForeignWord64,
    call "int16ToInt#" "aihc_int16_to_int" [GrinForeignWord64] GrinForeignWord64,
    call "intToInt32#" "aihc_int_to_int32" [GrinForeignWord64] GrinForeignWord64,
    call "int32ToInt#" "aihc_int32_to_int" [GrinForeignWord64] GrinForeignWord64,
    call "intToInt64#" "aihc_int_to_int64" [GrinForeignWord64] GrinForeignWord64,
    call "int64ToInt#" "aihc_int64_to_int" [GrinForeignWord64] GrinForeignWord64,
    call "plusFloat#" "aihc_float_plus" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "minusFloat#" "aihc_float_minus" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "timesFloat#" "aihc_float_times" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "negateFloat#" "aihc_float_negate" [GrinForeignWord64] GrinForeignWord64,
    call "fabsFloat#" "aihc_float_abs" [GrinForeignWord64] GrinForeignWord64,
    call "int2Float#" "aihc_int_to_float" [GrinForeignWord64] GrinForeignWord64,
    call "float2Int#" "aihc_float_to_int" [GrinForeignWord64] GrinForeignWord64,
    call "gtFloat#" "aihc_float_gt" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "ltFloat#" "aihc_float_lt" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "eqFloat#" "aihc_float_eq" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "+##" "aihc_double_plus" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "-##" "aihc_double_minus" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "*##" "aihc_double_times" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "negateDouble#" "aihc_double_negate" [GrinForeignWord64] GrinForeignWord64,
    call "fabsDouble#" "aihc_double_abs" [GrinForeignWord64] GrinForeignWord64,
    call "int2Double#" "aihc_int_to_double" [GrinForeignWord64] GrinForeignWord64,
    call "double2Int#" "aihc_double_to_int" [GrinForeignWord64] GrinForeignWord64,
    call ">##" "aihc_double_gt" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "<##" "aihc_double_lt" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "==##" "aihc_double_eq" [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "float2Double#" "aihc_float_to_double" [GrinForeignWord64] GrinForeignWord64,
    call "double2Float#" "aihc_double_to_float" [GrinForeignWord64] GrinForeignWord64,
    call "byteSwap#" "aihc_word_byte_swap64" [GrinForeignWord64] GrinForeignWord64,
    call "byteSwap16#" "aihc_word_byte_swap16" [GrinForeignWord64] GrinForeignWord64,
    call "byteSwap32#" "aihc_word_byte_swap32" [GrinForeignWord64] GrinForeignWord64,
    call "byteSwap64#" "aihc_word_byte_swap64" [GrinForeignWord64] GrinForeignWord64,
    -- A Float# value travels as its bit pattern in the low 32 bits and a
    -- Double# value as its 64-bit pattern, thus the unaligned float accessors
    -- reuse the word accessors of the same width.
    call "indexWord8OffAddrAsFloat#" "aihc_addr_index_byte_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8OffAddrAsDouble#" "aihc_addr_index_byte_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord8OffAddrAsFloat#" "aihc_addr_index_byte_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWord8OffAddrAsDouble#" "aihc_addr_index_byte_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord8OffAddrAsFloat#" "aihc_addr_write_byte_word32" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    procedure "writeWord8OffAddrAsDouble#" "aihc_addr_write_byte_word64" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "indexCharArray#" "aihc_byte_array_index_byte_word8" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8ArrayAsWord16#" "aihc_byte_array_index_byte_word16" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8ArrayAsWord32#" "aihc_byte_array_index_byte_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord8ArrayAsWord64#" "aihc_byte_array_index_byte_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64
  ]
  where
    call = runtimeCall False 1
    -- casMutVar# returns a failure flag and the final contents. The runtime
    -- function returns only the flag. Runtime calls do not yield, so each
    -- backend reads the final contents with readMutVar# directly after the swap.
    pairCall = runtimeCall False 2
    procedure = runtimeCall False 0
    machineCall = runtimeCall True 1

-- | Primitives whose GRIN result tuple comes from more than one runtime call.
-- Each call gets the same arguments and gives one result. The calls follow the
-- order of the result tuple.
nativeSplitRuntimePrimitiveCall :: Text -> Maybe [NativeRuntimeCall]
nativeSplitRuntimePrimitiveCall name = lookup name nativeSplitRuntimePrimitiveCalls

nativeSplitRuntimePrimitiveCalls :: [(Text, [NativeRuntimeCall])]
nativeSplitRuntimePrimitiveCalls =
  [ ( "timesInt2#",
      [ resultCall "aihc_int_times2_high_needed",
        resultCall "aihc_int_times2_high",
        resultCall "aihc_int_times2_low"
      ]
    )
  ]
  where
    resultCall symbol =
      snd (runtimeCall False 1 "timesInt2#" symbol [GrinForeignWord64, GrinForeignWord64] GrinForeignWord64)

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
