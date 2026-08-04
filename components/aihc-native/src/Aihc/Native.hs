{-# LANGUAGE OverloadedStrings #-}

-- | Architecture-neutral support shared by backend code generators.
module Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeTarget (..),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    backendCompiler,
    LinkInterface (..),
    LinkLayout (..),
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    buildAddrLiteralPool,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    hostNativeTarget,
    nativeTargetTriple,
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
    parseNativeTarget,
    renderNativeTarget,
    runtimePlan,
    snapshotSourcePath,
    supportedNativePrimitiveNames,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep)
import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Paths_aihc_native (getDataFileName)
import System.FilePath (takeDirectory)
import System.Info qualified as System

-- | A complete backend and executable target.
data NativeTarget
  = AppleArm64
  | LinuxAmd64
  | Llvm
  | Wasm32Wasip3
  deriving (Bounded, Enum, Eq, Ord, Show)

data RuntimeGarbageCollector
  = RuntimeGcCalloc
  | RuntimeGcSemispace
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

-- | Select the compiler driver and target arguments.
backendCompiler :: NativeTarget -> IO (FilePath, [String])
backendCompiler target =
  case target of
    Llvm -> pure ("clang", ["-Wno-override-module", "-O2"])
    Wasm32Wasip3 -> pure ("clang", ["--target=wasm32-unknown-unknown"])
    AppleArm64 -> nativeCompiler
    LinuxAmd64 -> nativeCompiler
  where
    nativeCompiler = pure ("clang", ["--target=" <> nativeTargetTriple target])

-- | The process-wide constructor tags and global table slots shared by all
-- compilation units in one executable.
data LinkLayout = LinkLayout
  { linkConstructors :: ![(Text, [[RuntimeRep]])],
    linkGlobalNames :: ![Text]
  }
  deriving (Eq, Show)

-- | Constructor and global-table metadata exported by a compilation
-- unit. Code generation for another unit never needs its GRIN bodies.
data LinkInterface = LinkInterface
  { linkInterfaceConstructors :: ![(Text, [[RuntimeRep]])],
    linkInterfaceGlobalNames :: ![Text]
  }
  deriving (Eq, Show, Read)

buildLinkLayout :: [GrinProgram] -> LinkLayout
buildLinkLayout = buildLinkLayoutFromInterfaces . map extractLinkInterface

buildLinkLayoutFromInterfaces :: [LinkInterface] -> LinkLayout
buildLinkLayoutFromInterfaces = foldl extendLinkLayoutWithInterface emptyLinkLayout

-- | Deduplicate address literals and assign short, unit-local assembly labels.
buildAddrLiteralPool :: GrinProgram -> [(ByteString, Text)]
buildAddrLiteralPool program =
  [ (value, ".Laihc_addr_" <> T.pack (show index))
  | (index, value) <- zip [0 :: Int ..] values
  ]
  where
    values = Set.toAscList (Set.fromList [value | GrinLitAddr value <- grinProgramLiterals program])

extractLinkInterface :: GrinProgram -> LinkInterface
extractLinkInterface program =
  LinkInterface
    { linkInterfaceConstructors = grinConstructors program,
      linkInterfaceGlobalNames = programGlobalNames program
    }

extendLinkLayout :: LinkLayout -> GrinProgram -> LinkLayout
extendLinkLayout layout = extendLinkLayoutWithInterface layout . extractLinkInterface

extendLinkLayoutWithInterface :: LinkLayout -> LinkInterface -> LinkLayout
extendLinkLayoutWithInterface layout interface =
  LinkLayout
    { linkConstructors = uniqueByName (linkConstructors layout <> linkInterfaceConstructors interface),
      linkGlobalNames = uniqueTexts (linkGlobalNames layout <> linkInterfaceGlobalNames interface)
    }

runtimeSourcePath :: IO FilePath
runtimeSourcePath = getDataFileName "runtime/aihc_runtime.c"

runtimePlan :: NativeTarget -> RuntimeGarbageCollector -> IO RuntimePlan
runtimePlan target garbageCollector = do
  core <- runtimeSourcePath
  collector <-
    getDataFileName $ case garbageCollector of
      RuntimeGcCalloc -> "runtime/aihc_gc_calloc.c"
      RuntimeGcSemispace -> "runtime/aihc_gc_semispace.c"
  host <-
    getDataFileName $ case target of
      Wasm32Wasip3 -> "runtime/aihc_host_wasip3.c"
      _ -> "runtime/aihc_host_posix.c"
  trampoline <- getDataFileName "runtime/aihc_runtime_trampoline.c"
  pure
    RuntimePlan
      { runtimeSources =
          [core, collector, host]
            <> [trampoline | target == Wasm32Wasip3],
        runtimeIncludeDirectories = [takeDirectory core]
      }

snapshotSourcePath :: IO FilePath
snapshotSourcePath = getDataFileName "runtime/aihc_snapshot.c"

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
    "eqWord#",
    "neWord#",
    "ltWord#",
    "leWord#",
    "gtWord#",
    "geWord#",
    "realWorld#",
    "newArray#",
    "unsafeFreezeArray#",
    "unsafeThawArray#",
    "unsafeFreezeByteArray#",
    "unsafeThawByteArray#"
  ]
    <> map fst nativeCpsPrimitiveCalls
    <> map fst nativeRuntimePrimitiveCalls

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
nativeRuntimePrimitiveCall :: Text -> Maybe GrinForeignCall
nativeRuntimePrimitiveCall name = lookup name nativeRuntimePrimitiveCalls

nativeRuntimePrimitiveCalls :: [(Text, GrinForeignCall)]
nativeRuntimePrimitiveCalls =
  [ call "indexWord8OffAddr#" "aihc_addr_index_word8" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord32OffAddr#" "aihc_addr_index_word32" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexWord64OffAddr#" "aihc_addr_index_word64" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "indexArray#" "aihc_array_index" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readArray#" "aihc_array_index" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "writeArray#" "aihc_array_write" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "sameMutableArray#" "aihc_array_same" [GrinForeignAddr, GrinForeignAddr] GrinForeignWord64,
    call "newByteArray#" "aihc_byte_array_new" [GrinForeignWord64] GrinForeignAddr,
    call "newPinnedByteArray#" "aihc_byte_array_new_pinned" [GrinForeignWord64] GrinForeignAddr,
    call "newAlignedPinnedByteArray#" "aihc_byte_array_new_aligned_pinned" [GrinForeignWord64, GrinForeignWord64] GrinForeignAddr,
    call "isMutableByteArrayPinned#" "aihc_byte_array_is_pinned" [GrinForeignAddr] GrinForeignWord64,
    call "isByteArrayPinned#" "aihc_byte_array_is_pinned" [GrinForeignAddr] GrinForeignWord64,
    call "byteArrayContents#" "aihc_byte_array_contents" [GrinForeignAddr] GrinForeignAddr,
    call "mutableByteArrayContents#" "aihc_byte_array_contents" [GrinForeignAddr] GrinForeignAddr,
    call "shrinkMutableByteArray#" "aihc_byte_array_shrink" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "resizeMutableByteArray#" "aihc_byte_array_resize" [GrinForeignAddr, GrinForeignWord64] GrinForeignAddr,
    call "sizeofByteArray#" "aihc_byte_array_get_size" [GrinForeignAddr] GrinForeignWord64,
    call "getSizeofMutableByteArray#" "aihc_byte_array_get_size" [GrinForeignAddr] GrinForeignWord64,
    call "copyAddrToByteArray#" "aihc_byte_array_copy_from_addr" [GrinForeignAddr, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "indexWordArray#" "aihc_byte_array_index_word" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "readWordArray#" "aihc_byte_array_read_word" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64,
    call "writeWordArray#" "aihc_byte_array_write_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "copyByteArray#" "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64,
    call "clz#" "aihc_word_clz" [GrinForeignWord64] GrinForeignWord64,
    call "ctz#" "aihc_word_ctz" [GrinForeignWord64] GrinForeignWord64,
    call "popCnt#" "aihc_word_popcount" [GrinForeignWord64] GrinForeignWord64
  ]
  where
    call primitive symbol arguments result =
      ( primitive,
        GrinForeignCall
          { grinForeignCallName = "$runtime$" <> symbol,
            grinForeignCallSymbol = symbol,
            grinForeignCallSignature =
              GrinForeignSignature
                { grinForeignArgumentTypes = arguments,
                  grinForeignResultType = result,
                  grinForeignEffect = GrinForeignPure
                }
          }
      )

emptyLinkLayout :: LinkLayout
emptyLinkLayout =
  LinkLayout
    { linkConstructors = builtinConstructors,
      linkGlobalNames = [name | (name, layouts) <- builtinConstructors, null layouts]
    }

programGlobalNames :: GrinProgram -> [Text]
programGlobalNames program =
  [name | (name, arity) <- programConstructorArities program, arity == 0]
    <> map (grinVarName . fst) (grinWhnfGlobals program)
    <> map (grinVarName . fst) (grinCafs program)

programConstructorArities :: GrinProgram -> [(Text, Int)]
programConstructorArities program =
  [(name, length fieldLayouts) | (name, fieldLayouts) <- grinConstructors program]

uniqueTexts :: [Text] -> [Text]
uniqueTexts = reverse . snd . foldl' step (Set.empty, [])
  where
    step (seen, values) value
      | value `Set.member` seen = (seen, values)
      | otherwise = (Set.insert value seen, value : values)

uniqueByName :: [(Text, value)] -> [(Text, value)]
uniqueByName values =
  [ (name, arity)
  | name <- uniqueTexts (map fst values),
    Just arity <- [lookup name values]
  ]
