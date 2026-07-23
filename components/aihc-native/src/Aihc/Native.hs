{-# LANGUAGE OverloadedStrings #-}

-- | Architecture-neutral support shared by backend code generators.
module Aihc.Native
  ( NativeTarget (..),
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
    nativeRuntimePrimitiveCall,
    parseNativeTarget,
    renderNativeTarget,
    runtimeSourcePath,
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
import System.Info qualified as System

-- | A complete backend and executable target.
data NativeTarget
  = AppleArm64
  | LinuxAmd64
  | PortableC
  deriving (Bounded, Enum, Eq, Ord, Show)

renderNativeTarget :: NativeTarget -> String
renderNativeTarget target =
  case target of
    AppleArm64 -> "apple-arm64"
    LinuxAmd64 -> "linux-amd64"
    PortableC -> "portable-c"

parseNativeTarget :: String -> Either String NativeTarget
parseNativeTarget value =
  case value of
    "apple-arm64" -> Right AppleArm64
    "arm64-apple-darwin" -> Right AppleArm64
    "linux-amd64" -> Right LinuxAmd64
    "x86_64-unknown-linux-gnu" -> Right LinuxAmd64
    "portable-c" -> Right PortableC
    "c" -> Right PortableC
    _ -> Left "target must be apple-arm64, linux-amd64, or portable-c"

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
    PortableC -> "portable-c"

-- | Select the C or assembly compiler driver and target arguments.
backendCompiler :: NativeTarget -> IO (FilePath, [String])
backendCompiler target =
  case target of
    PortableC -> pure ("clang", [])
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

snapshotSourcePath :: IO FilePath
snapshotSourcePath = getDataFileName "runtime/aihc_snapshot.c"

-- | Primitive operations implemented directly by every native backend or by
-- the shared runtime ABI.
supportedNativePrimitiveNames :: [Text]
supportedNativePrimitiveNames =
  [ "+#",
    "-#",
    "*#",
    "<#",
    "==#",
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
    "eqWord#",
    "neWord#",
    "ltWord#",
    "leWord#",
    "gtWord#",
    "geWord#",
    "clz#",
    "ctz#",
    "popCnt#",
    "awaitIO#",
    "fork#",
    "realWorld#",
    "yield#",
    "newByteArray#",
    "newPinnedByteArray#",
    "newAlignedPinnedByteArray#",
    "isMutableByteArrayPinned#",
    "isByteArrayPinned#",
    "byteArrayContents#",
    "mutableByteArrayContents#",
    "shrinkMutableByteArray#",
    "resizeMutableByteArray#",
    "unsafeFreezeByteArray#",
    "unsafeThawByteArray#",
    "sizeofByteArray#",
    "getSizeofMutableByteArray#",
    "copyAddrToByteArray#",
    "indexWordArray#",
    "readWordArray#",
    "writeWordArray#",
    "copyByteArray#"
  ]

-- | Runtime call used to implement a byte-array primitive. Freeze and thaw are
-- representation-preserving and therefore deliberately have no runtime call.
nativeRuntimePrimitiveCall :: Text -> Maybe GrinForeignCall
nativeRuntimePrimitiveCall name =
  case name of
    "newByteArray#" -> call "aihc_byte_array_new" [GrinForeignWord64] GrinForeignAddr
    "newPinnedByteArray#" -> call "aihc_byte_array_new_pinned" [GrinForeignWord64] GrinForeignAddr
    "newAlignedPinnedByteArray#" -> call "aihc_byte_array_new_aligned_pinned" [GrinForeignWord64, GrinForeignWord64] GrinForeignAddr
    "isMutableByteArrayPinned#" -> call "aihc_byte_array_is_pinned" [GrinForeignAddr] GrinForeignWord64
    "isByteArrayPinned#" -> call "aihc_byte_array_is_pinned" [GrinForeignAddr] GrinForeignWord64
    "byteArrayContents#" -> call "aihc_byte_array_contents" [GrinForeignAddr] GrinForeignAddr
    "mutableByteArrayContents#" -> call "aihc_byte_array_contents" [GrinForeignAddr] GrinForeignAddr
    "shrinkMutableByteArray#" -> call "aihc_byte_array_shrink" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64
    "resizeMutableByteArray#" -> call "aihc_byte_array_resize" [GrinForeignAddr, GrinForeignWord64] GrinForeignAddr
    "sizeofByteArray#" -> call "aihc_byte_array_get_size" [GrinForeignAddr] GrinForeignWord64
    "getSizeofMutableByteArray#" -> call "aihc_byte_array_get_size" [GrinForeignAddr] GrinForeignWord64
    "copyAddrToByteArray#" -> call "aihc_byte_array_copy_from_addr" [GrinForeignAddr, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64
    "indexWordArray#" -> call "aihc_byte_array_index_word" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64
    "readWordArray#" -> call "aihc_byte_array_read_word" [GrinForeignAddr, GrinForeignWord64] GrinForeignWord64
    "writeWordArray#" -> call "aihc_byte_array_write_word" [GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64
    "copyByteArray#" -> call "aihc_byte_array_copy" [GrinForeignAddr, GrinForeignWord64, GrinForeignAddr, GrinForeignWord64, GrinForeignWord64] GrinForeignWord64
    "clz#" -> call "aihc_word_clz" [GrinForeignWord64] GrinForeignWord64
    "ctz#" -> call "aihc_word_ctz" [GrinForeignWord64] GrinForeignWord64
    "popCnt#" -> call "aihc_word_popcount" [GrinForeignWord64] GrinForeignWord64
    _ -> Nothing
  where
    call symbol arguments result =
      Just
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
