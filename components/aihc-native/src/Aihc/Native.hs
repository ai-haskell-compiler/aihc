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
    parseNativeTarget,
    renderNativeTarget,
    runtimeSourcePath,
    snapshotSourcePath,
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
  | Wasm32Wasip3
  deriving (Bounded, Enum, Eq, Ord, Show)

renderNativeTarget :: NativeTarget -> String
renderNativeTarget target =
  case target of
    AppleArm64 -> "apple-arm64"
    LinuxAmd64 -> "linux-amd64"
    PortableC -> "portable-c"
    Wasm32Wasip3 -> "wasm32-wasip3"

parseNativeTarget :: String -> Either String NativeTarget
parseNativeTarget value =
  case value of
    "apple-arm64" -> Right AppleArm64
    "arm64-apple-darwin" -> Right AppleArm64
    "linux-amd64" -> Right LinuxAmd64
    "x86_64-unknown-linux-gnu" -> Right LinuxAmd64
    "portable-c" -> Right PortableC
    "c" -> Right PortableC
    "wasm32-wasip3" -> Right Wasm32Wasip3
    "wasip3" -> Right Wasm32Wasip3
    _ -> Left "target must be apple-arm64, linux-amd64, portable-c, or wasm32-wasip3"

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
    Wasm32Wasip3 -> "wasm32-unknown-unknown"

-- | Select the C or assembly compiler driver and target arguments.
backendCompiler :: NativeTarget -> IO (FilePath, [String])
backendCompiler target =
  case target of
    PortableC -> pure ("clang", [])
    Wasm32Wasip3 -> pure ("clang", ["--target=wasm32-unknown-unknown"])
    AppleArm64 -> nativeCompiler
    LinuxAmd64 -> nativeCompiler
  where
    nativeCompiler = pure ("clang", ["--target=" <> nativeTargetTriple target])

-- | The process-wide constructor tags and global table slots shared by all
-- compilation units in one executable.
data LinkLayout = LinkLayout
  { linkConstructors :: ![(Text, [[RuntimeRep]])],
    linkGlobalNames :: ![Text],
    linkMaximumArgumentSlots :: !Int
  }
  deriving (Eq, Show)

-- | Constructor and global-table metadata exported by a compilation
-- unit. Code generation for another unit never needs its GRIN bodies.
data LinkInterface = LinkInterface
  { linkInterfaceConstructors :: ![(Text, [[RuntimeRep]])],
    linkInterfaceGlobalNames :: ![Text],
    linkInterfaceMaximumArgumentSlots :: !Int
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
      linkInterfaceGlobalNames = programGlobalNames program,
      linkInterfaceMaximumArgumentSlots =
        maximum (0 : map (length . grinFunctionParameters) (grinFunctions program))
    }

extendLinkLayout :: LinkLayout -> GrinProgram -> LinkLayout
extendLinkLayout layout = extendLinkLayoutWithInterface layout . extractLinkInterface

extendLinkLayoutWithInterface :: LinkLayout -> LinkInterface -> LinkLayout
extendLinkLayoutWithInterface layout interface =
  LinkLayout
    { linkConstructors = uniqueByName (linkConstructors layout <> linkInterfaceConstructors interface),
      linkGlobalNames = uniqueTexts (linkGlobalNames layout <> linkInterfaceGlobalNames interface),
      linkMaximumArgumentSlots = max (linkMaximumArgumentSlots layout) (linkInterfaceMaximumArgumentSlots interface)
    }

runtimeSourcePath :: IO FilePath
runtimeSourcePath = getDataFileName "runtime/aihc_runtime.c"

snapshotSourcePath :: IO FilePath
snapshotSourcePath = getDataFileName "runtime/aihc_snapshot.c"

emptyLinkLayout :: LinkLayout
emptyLinkLayout =
  LinkLayout
    { linkConstructors = builtinConstructors,
      linkGlobalNames = [name | (name, layouts) <- builtinConstructors, null layouts],
      linkMaximumArgumentSlots = 0
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
