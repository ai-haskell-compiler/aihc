-- | The vocabulary that the aihc tools use for Cabal packages: package
-- names, flags, platforms, versions, and version ranges.
--
-- "Aihc.Cabal" parses the Cabal files, the dependencies, and the version
-- ranges. It keeps names as text and it does not know the host platform.
-- This module gives the names their own types, gives 'String' forms of the
-- parsers for the command line, and names the platforms of the targets.
module Aihc.Hackage.Package
  ( -- * Cabal files
    parsePackageDescription,
    packageNameOf,

    -- * Package names
    PackageName,
    mkPackageName,
    unPackageName,
    packageNameText,

    -- * Flags
    FlagName,
    mkFlagName,
    unFlagName,
    FlagAssignment,
    mkFlagAssignment,
    unFlagAssignment,
    lookupFlagAssignment,

    -- * Platforms
    OS (..),
    Arch (..),
    buildOS,
    buildArch,
    osName,
    archName,

    -- * Versions
    Version,
    versionFromList,
    versionToList,
    showVersion,
    parseVersionString,
    parsePackageIdentifier,

    -- * Version ranges
    VersionRange,
    anyVersion,
    noVersion,
    thisVersion,
    withinRange,
    intersectVersionRanges,
    simplifyVersionRange,
    showVersionRange,
    parseVersionRangeString,
    parseDependencyString,
  )
where

import Aihc.Cabal (FlagAssignment, Package, ParseResult (..), Version, VersionRange (..), anyVersion, mkVersion, noVersion, parsePackage, parseVersion, parseVersionRange, thisVersion, versionNumbers, withinRange)
import Aihc.Cabal qualified as Cabal
import Data.ByteString qualified as BS
import Data.Char (isDigit, isSpace, toLower)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.Info qualified as Info

-- | Parse the contents of a @.cabal@ file.
parsePackageDescription :: BS.ByteString -> Either String Package
parsePackageDescription bytes =
  case parseValue (parsePackage bytes) of
    Right package -> Right package
    Left diagnostic -> Left (T.unpack (Cabal.renderDiagnostic diagnostic))

-- | The name of a package.
newtype PackageName = PackageName Text
  deriving (Eq, Ord)

instance Show PackageName where
  showsPrec precedence (PackageName name) =
    showParen (precedence > 10) (showString "PackageName " . shows name)

mkPackageName :: String -> PackageName
mkPackageName = PackageName . T.pack

unPackageName :: PackageName -> String
unPackageName (PackageName name) = T.unpack name

packageNameText :: PackageName -> Text
packageNameText (PackageName name) = name

packageNameOf :: Package -> PackageName
packageNameOf = PackageName . Cabal.packageName

-- | The name of a flag. The parser gives flag names in lower case.
type FlagName = Text

-- | Flag names are not case-sensitive, so this gives them in lower case.
mkFlagName :: String -> FlagName
mkFlagName = T.toLower . T.pack

unFlagName :: FlagName -> String
unFlagName = T.unpack

mkFlagAssignment :: [(FlagName, Bool)] -> FlagAssignment
mkFlagAssignment = Map.fromList

-- | The flags in name order.
unFlagAssignment :: FlagAssignment -> [(FlagName, Bool)]
unFlagAssignment = Map.toAscList

lookupFlagAssignment :: FlagName -> FlagAssignment -> Maybe Bool
lookupFlagAssignment = Map.lookup

-- | The operating system of a target. 'osName' gives the name that the
-- conditions of a Cabal file compare with.
data OS
  = Linux
  | OSX
  | Windows
  | FreeBSD
  | OpenBSD
  | NetBSD
  | Wasi
  | OtherOS String
  deriving (Eq, Ord, Show)

-- | The architecture of a target. 'archName' gives the name that the
-- conditions of a Cabal file compare with.
data Arch
  = X86_64
  | AArch64
  | I386
  | Arm
  | Wasm32
  | JavaScript
  | OtherArch String
  deriving (Eq, Ord, Show)

-- | The name that Cabal gives to an operating system.
osName :: OS -> String
osName os =
  case os of
    Linux -> "linux"
    OSX -> "osx"
    Windows -> "windows"
    FreeBSD -> "freebsd"
    OpenBSD -> "openbsd"
    NetBSD -> "netbsd"
    Wasi -> "wasi"
    OtherOS name -> name

-- | The name that Cabal gives to an architecture.
archName :: Arch -> String
archName arch =
  case arch of
    X86_64 -> "x86_64"
    AArch64 -> "aarch64"
    I386 -> "i386"
    Arm -> "arm"
    Wasm32 -> "wasm32"
    JavaScript -> "javascript"
    OtherArch name -> name

-- | The operating system that this program runs on. "Aihc.Cabal" applies
-- the Cabal aliases for host names when it compares names, so only the
-- names of the constructors must be known here.
buildOS :: OS
buildOS =
  case map toLower Info.os of
    "linux" -> Linux
    "darwin" -> OSX
    "osx" -> OSX
    "mingw32" -> Windows
    "windows" -> Windows
    "freebsd" -> FreeBSD
    "openbsd" -> OpenBSD
    "netbsd" -> NetBSD
    "wasi" -> Wasi
    other -> OtherOS other

-- | The architecture that this program runs on.
buildArch :: Arch
buildArch =
  case map toLower Info.arch of
    "x86_64" -> X86_64
    "aarch64" -> AArch64
    "i386" -> I386
    "arm" -> Arm
    "wasm32" -> Wasm32
    "javascript" -> JavaScript
    other -> OtherArch other

-- | A version from its components. An empty list gives version @0@.
versionFromList :: [Int] -> Version
versionFromList components =
  case mkVersion (map toInteger (if null components then [0] else components)) of
    Just version -> version
    Nothing -> error ("versionFromList: negative component in " <> show components)

versionToList :: Version -> [Int]
versionToList = map fromInteger . NE.toList . versionNumbers

-- | A version in the Cabal notation, such as @1.2.3@.
showVersion :: Version -> String
showVersion = intercalate "." . map show . versionToList

parseVersionString :: String -> Maybe Version
parseVersionString text
  | not (null text) && all (\character -> isDigit character || character == '.') text = either (const Nothing) Just (parseVersion (T.pack text))
  | otherwise = Nothing

-- | Parse @NAME@ or @NAME-VERSION@, as Cabal reads a package identifier.
parsePackageIdentifier :: String -> Maybe (PackageName, Maybe Version)
parsePackageIdentifier text =
  either (const Nothing) (\(name, version) -> Just (PackageName name, version)) (Cabal.parsePackageIdentifier (T.pack text))

intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges = Both

-- | Parse a version range in the Cabal notation. An empty text gives
-- 'anyVersion'.
parseVersionRangeString :: String -> Maybe VersionRange
parseVersionRangeString text
  | all isSpace text = Just anyVersion
  | otherwise = either (const Nothing) Just (parseVersionRange (T.pack text))

-- | Parse a dependency such as @base >=4 && <5@ or @base>=4@, as Cabal reads
-- one @build-depends@ entry. Spaces around the text are permitted.
parseDependencyString :: String -> Maybe (PackageName, VersionRange)
parseDependencyString text =
  case Cabal.parseDependency (T.strip (T.pack text)) of
    Right dependency -> Just (PackageName (Cabal.dependencyPackage dependency), Cabal.dependencyRange dependency)
    Left _ -> Nothing

-- | The same versions as a union of separate intervals in increasing order.
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange = Cabal.simplifyVersionRange

-- | A range in the Cabal notation, such as @>=1.2 && <2@.
showVersionRange :: VersionRange -> String
showVersionRange = T.unpack . Cabal.renderVersionRange
