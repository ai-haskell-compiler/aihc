-- | The vocabulary that the aihc tools use for Cabal packages: package
-- names, flags, platforms, versions, and version ranges.
--
-- "Aihc.Cabal" parses the Cabal files. It keeps names as text and it does
-- not know the host platform. This module gives the names their own types,
-- reads and shows versions and ranges in the Cabal notation, and names the
-- platforms that the conditions of a Cabal file test.
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
    classifyOS,
    classifyArch,

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

import Aihc.Cabal (FlagAssignment, Package, ParseResult (..), Position (..), Version, VersionRange (..), anyVersion, mkVersion, noVersion, parsePackage, parseVersion, parseVersionRange, thisVersion, versionNumbers, withinRange)
import Aihc.Cabal qualified as Cabal
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.List (intercalate, sortOn)
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
    Left diagnostic -> Left (position (Cabal.diagnosticPosition diagnostic) <> T.unpack (Cabal.diagnosticMessage diagnostic))
  where
    position = maybe "" (\(Position row column) -> "line " <> show row <> ", column " <> show column <> ": ")

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

-- | An operating system that the condition @os(...)@ can test.
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

-- | An architecture that the condition @arch(...)@ can test.
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

-- | The operating system of a name, with the aliases that Cabal accepts.
classifyOS :: String -> OS
classifyOS name =
  case map toLower name of
    "linux" -> Linux
    "osx" -> OSX
    "darwin" -> OSX
    "windows" -> Windows
    "mingw32" -> Windows
    "win32" -> Windows
    "cygwin32" -> Windows
    "freebsd" -> FreeBSD
    "openbsd" -> OpenBSD
    "netbsd" -> NetBSD
    "wasi" -> Wasi
    other -> OtherOS other

-- | The architecture of a name, with the aliases that Cabal accepts.
classifyArch :: String -> Arch
classifyArch name =
  case map toLower name of
    "x86_64" -> X86_64
    "amd64" -> X86_64
    "x86-64" -> X86_64
    "aarch64" -> AArch64
    "arm64" -> AArch64
    "i386" -> I386
    "i486" -> I386
    "i586" -> I386
    "i686" -> I386
    "x86" -> I386
    "arm" -> Arm
    "wasm32" -> Wasm32
    "javascript" -> JavaScript
    other -> OtherArch other

-- | The operating system that this program runs on.
buildOS :: OS
buildOS = classifyOS Info.os

-- | The architecture that this program runs on.
buildArch :: Arch
buildArch = classifyArch Info.arch

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

-- | Parse @NAME@ or @NAME-VERSION@.
parsePackageIdentifier :: String -> Maybe (PackageName, Maybe Version)
parsePackageIdentifier text =
  case break (== '-') (reverse text) of
    (reversedVersion, '-' : reversedName)
      | Just version <- parseVersionString (reverse reversedVersion),
        validPackageName (reverse reversedName) ->
          Just (mkPackageName (reverse reversedName), Just version)
    _
      | validPackageName text -> Just (mkPackageName text, Nothing)
      | otherwise -> Nothing

validPackageName :: String -> Bool
validPackageName name =
  not (null name) && all validPart (splitOn '-' name)
  where
    validPart part = not (null part) && all isAlphaNum part && not (all isDigit part)

splitOn :: Char -> String -> [String]
splitOn separator text =
  case break (== separator) text of
    (part, []) -> [part]
    (part, _ : rest) -> part : splitOn separator rest

intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges = Both

-- | Parse a version range in the Cabal notation. An empty text gives
-- 'anyVersion'.
parseVersionRangeString :: String -> Maybe VersionRange
parseVersionRangeString text
  | all isSpace text = Just anyVersion
  | otherwise = either (const Nothing) Just (parseVersionRange (T.pack text))

-- | Parse a dependency such as @base >=4 && <5@ or @base>=4@.
parseDependencyString :: String -> Maybe (PackageName, VersionRange)
parseDependencyString text = do
  let (name, rest) = span (\character -> isAlphaNum character || character == '-') (dropWhile isSpace text)
  if validPackageName name
    then (,) (mkPackageName name) <$> parseVersionRangeString rest
    else Nothing

-- | One continuous part of a range: a lower bound and an upper bound. A
-- missing upper bound means no limit. The lower bound of every range is at
-- least version @0@, the smallest version.
data Interval = Interval !Bound !(Maybe Bound)
  deriving (Eq)

-- | A version, and whether the version itself is in the interval.
data Bound = Bound !Version !Bool
  deriving (Eq)

-- | The same set of versions as disjoint intervals in increasing order.
intervals :: VersionRange -> [Interval]
intervals range =
  case range of
    AnyVersion -> [Interval zeroBound Nothing]
    Equal version -> [Interval (Bound version True) (Just (Bound version True))]
    Later version -> [Interval (Bound version False) Nothing]
    Earlier version -> normalize [Interval zeroBound (Just (Bound version False))]
    AtLeast version -> [Interval (Bound version True) Nothing]
    AtMost version -> normalize [Interval zeroBound (Just (Bound version True))]
    MajorBound version -> [Interval (Bound version True) (Just (Bound (majorUpperBound version) False))]
    EitherRange left right -> normalize (intervals left <> intervals right)
    Both left right -> normalize [both a b | a <- intervals left, b <- intervals right]
  where
    zeroBound = Bound (versionFromList [0]) True
    both (Interval low high) (Interval low' high') = Interval (maxLower low low') (minUpper high high')
    maxLower a@(Bound x xIn) b@(Bound y yIn)
      | x > y = a
      | y > x = b
      | otherwise = Bound x (xIn && yIn)
    minUpper Nothing b = b
    minUpper a Nothing = a
    minUpper (Just a@(Bound x xIn)) (Just b@(Bound y yIn))
      | x < y = Just a
      | y < x = Just b
      | otherwise = Just (Bound x (xIn && yIn))

normalize :: [Interval] -> [Interval]
normalize = merge . sortOn lowerKey . filter nonEmpty
  where
    lowerKey (Interval (Bound version inclusive) _) = (version, not inclusive)
    merge (first : second : rest)
      | touches first second = merge (union first second : rest)
      | otherwise = first : merge (second : rest)
    merge rest = rest
    touches (Interval _ Nothing) _ = True
    touches (Interval _ (Just (Bound upper upperIn))) (Interval (Bound lower lowerIn) _) =
      lower < upper || (lower == upper && (upperIn || lowerIn))
    union (Interval low high) (Interval _ high') = Interval low (maxUpper high high')
    maxUpper Nothing _ = Nothing
    maxUpper _ Nothing = Nothing
    maxUpper (Just a@(Bound x xIn)) (Just b@(Bound y yIn))
      | x > y = Just a
      | y > x = Just b
      | otherwise = Just (Bound x (xIn || yIn))

nonEmpty :: Interval -> Bool
nonEmpty (Interval _ Nothing) = True
nonEmpty (Interval (Bound lower lowerIn) (Just (Bound upper upperIn))) =
  lower < upper || (lower == upper && lowerIn && upperIn)

-- | The same set of versions in a canonical form: a union of disjoint
-- intervals in increasing order.
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange range =
  case map fromInterval (intervals range) of
    [] -> noVersion
    first : rest -> foldl EitherRange first rest
  where
    zero = versionFromList [0]
    fromInterval (Interval (Bound lower lowerIn) upper)
      | lowerIn, upper == Just (Bound lower True) = thisVersion lower
      | otherwise =
          case (lowerRange, upperRange) of
            (Nothing, Nothing) -> anyVersion
            (Just low, Nothing) -> low
            (Nothing, Just high) -> high
            (Just low, Just high) -> Both low high
      where
        lowerRange
          | lowerIn && lower == zero = Nothing
          | lowerIn = Just (AtLeast lower)
          | otherwise = Just (Later lower)
        upperRange = fmap (\(Bound version inclusive) -> if inclusive then AtMost version else Earlier version) upper

-- | A range in the notation that Cabal shows, such as @>=1.2 && <2@.
showVersionRange :: VersionRange -> String
showVersionRange = go (0 :: Int)
  where
    go precedence range =
      case range of
        AnyVersion -> ">=0"
        Equal version -> "==" <> showVersion version
        Later version -> ">" <> showVersion version
        Earlier version -> "<" <> showVersion version
        AtLeast version -> ">=" <> showVersion version
        AtMost version -> "<=" <> showVersion version
        MajorBound version -> "^>=" <> showVersion version
        Both left right -> parenthesize (precedence > 1) (go 1 left <> " && " <> go 2 right)
        EitherRange left right -> parenthesize (precedence > 0) (go 0 left <> " || " <> go 1 right)
    parenthesize True text = "(" <> text <> ")"
    parenthesize False text = text

-- | The first version after the major version of @^>=@: @1.3@ for @^>=1.2.4@.
majorUpperBound :: Version -> Version
majorUpperBound version =
  case versionToList version of
    [major] -> versionFromList [major, 1]
    major : minor : _ -> versionFromList [major, minor + 1]
    [] -> versionFromList [1]
