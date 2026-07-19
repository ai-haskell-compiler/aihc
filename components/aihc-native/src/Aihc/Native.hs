-- | Architecture-neutral support shared by native code generators.
module Aihc.Native
  ( NativeTarget (..),
    LinkInterface (..),
    LinkLayout (..),
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
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
import Data.Set qualified as Set
import Data.Text (Text)
import Paths_aihc_native (getDataFileName)
import System.Info qualified as System

-- | A complete native assembly and executable target.
data NativeTarget
  = AppleArm64
  | LinuxAmd64
  deriving (Bounded, Enum, Eq, Ord, Show)

renderNativeTarget :: NativeTarget -> String
renderNativeTarget target =
  case target of
    AppleArm64 -> "apple-arm64"
    LinuxAmd64 -> "linux-amd64"

parseNativeTarget :: String -> Either String NativeTarget
parseNativeTarget value =
  case value of
    "apple-arm64" -> Right AppleArm64
    "arm64-apple-darwin" -> Right AppleArm64
    "linux-amd64" -> Right LinuxAmd64
    "x86_64-unknown-linux-gnu" -> Right LinuxAmd64
    _ -> Left "target must be apple-arm64 or linux-amd64"

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

-- | The process-wide constructor tags and global table slots shared by all
-- native compilation units in one executable.
data LinkLayout = LinkLayout
  { linkConstructors :: ![(Text, Int)],
    linkGlobalNames :: ![Text]
  }
  deriving (Eq, Show)

-- | Constructor and global-table metadata exported by a native compilation
-- unit. Code generation for another unit never needs its GRIN bodies.
data LinkInterface = LinkInterface
  { linkInterfaceConstructors :: ![(Text, Int)],
    linkInterfaceGlobalNames :: ![Text]
  }
  deriving (Eq, Show, Read)

buildLinkLayout :: [GrinProgram] -> LinkLayout
buildLinkLayout = buildLinkLayoutFromInterfaces . map extractLinkInterface

buildLinkLayoutFromInterfaces :: [LinkInterface] -> LinkLayout
buildLinkLayoutFromInterfaces = foldl extendLinkLayoutWithInterface emptyLinkLayout

extractLinkInterface :: GrinProgram -> LinkInterface
extractLinkInterface program =
  LinkInterface
    { linkInterfaceConstructors = programConstructorArities program,
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

emptyLinkLayout :: LinkLayout
emptyLinkLayout =
  LinkLayout
    { linkConstructors = [(name, length layouts) | (name, layouts) <- builtinConstructors],
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

uniqueByName :: [(Text, Int)] -> [(Text, Int)]
uniqueByName values =
  [ (name, arity)
  | name <- uniqueTexts (map fst values),
    Just arity <- [lookup name values]
  ]
