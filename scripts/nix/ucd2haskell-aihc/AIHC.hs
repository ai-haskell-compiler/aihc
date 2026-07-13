{-# LANGUAGE ImportQualifiedPost #-}

module UCD2Haskell.AIHC (genAihcModule) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Short qualified as BS
import Data.Char (ord)
import Data.List (intercalate, sortOn)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath (takeDirectory, (</>))
import Unicode.CharacterDatabase.Parser.Common qualified as C
import Unicode.CharacterDatabase.Parser.Properties.Multiple qualified as P
import Unicode.CharacterDatabase.Parser.UnicodeData qualified as UD

data Range a = Range
  { rangeStart :: !Int,
    rangeEnd :: !Int,
    rangeValue :: a
  }
  deriving (Eq, Show)

data Mapping = Identity | Delta !Int
  deriving (Eq, Show)

maxCodePoint :: Int
maxCodePoint = 0x10ffff

genAihcModule :: FilePath -> FilePath -> IO ()
genAihcModule inputDirectory outputDirectory = do
  version <- getEnv "UNICODE_VERSION"
  unicodeData <- UD.parse <$> B.readFile (inputDirectory </> "UnicodeData.txt")
  derivedProperties <- P.parse <$> B.readFile (inputDirectory </> "DerivedCoreProperties.txt")
  let output = outputDirectory </> "GHC" </> "Prim" </> "Unicode.hs"
  createDirectoryIfMissing True (takeDirectory output)
  writeFile output (generateModule version unicodeData derivedProperties)

generateModule :: String -> [UD.Entry] -> [P.Entry] -> String
generateModule version unicodeData derivedProperties =
  let categories = completeRanges (fromEnum UD.Cn) (map categoryRange unicodeData)
      uppercase = propertyRanges "Uppercase" derivedProperties
      lowercase = propertyRanges "Lowercase" derivedProperties
      upperMappings = mappingRanges UD.simpleUpperCaseMapping unicodeData
      lowerMappings = mappingRanges UD.simpleLowerCaseMapping unicodeData
      titleMappings = mappingRanges UD.simpleTitleCaseMapping unicodeData
   in renderHeader version (parseVersion version)
        <> renderLookup "generalCategoryCode#" categories (\tag -> show tag <> "#")
        <> renderLookup "isUppercaseCode#" uppercase renderBit
        <> renderLookup "isLowercaseCode#" lowercase renderBit
        <> renderLookup "toUpperCode#" upperMappings renderMapping
        <> renderLookup "toLowerCode#" lowerMappings renderMapping
        <> renderLookup "toTitleCode#" titleMappings renderMapping

categoryRange :: UD.Entry -> Range Int
categoryRange (UD.Entry codePoints details) =
  fmapRange (const (fromEnum (UD.generalCategory details))) (codePointRange codePoints ())

propertyRanges :: String -> [P.Entry] -> [Range Bool]
propertyRanges propertyName entries =
  completeRanges
    False
    [ codePointRange codePoints True
    | P.Entry codePoints property _ <- entries,
      property == BS.toShort (B8.pack propertyName)
    ]

mappingRanges :: (UD.CharDetails -> Maybe Char) -> [UD.Entry] -> [Range Mapping]
mappingRanges selectMapping entries =
  completeRanges Identity . compressMappings . sortOn fst $
    [ (ord source, ord target)
    | UD.Entry codePoints details <- entries,
      C.SingleChar source <- [codePoints],
      Just target <- [selectMapping details],
      target /= source
    ]

codePointRange :: C.CodePointRange -> a -> Range a
codePointRange codePoints value =
  case codePoints of
    C.SingleChar char -> Range (ord char) (ord char) value
    C.CharRange start end -> Range (ord start) (ord end) value

compressMappings :: [(Int, Int)] -> [Range Mapping]
compressMappings [] = []
compressMappings ((source, target) : rest) = go source source (target - source) rest
  where
    go start previous delta [] = [Range start previous (Delta delta)]
    go start previous delta ((nextSource, nextTarget) : more)
      | nextSource == previous + 1,
        nextTarget - nextSource == delta =
          go start nextSource delta more
      | otherwise =
          Range start previous (Delta delta)
            : go nextSource nextSource (nextTarget - nextSource) more

completeRanges :: (Eq a) => a -> [Range a] -> [Range a]
completeRanges defaultValue ranges = mergeAdjacent (go 0 (sortOn rangeStart ranges))
  where
    go position []
      | position <= maxCodePoint = [Range position maxCodePoint defaultValue]
      | otherwise = []
    go position (range : rest)
      | rangeStart range < position =
          error ("overlapping Unicode ranges at code point " <> show (rangeStart range))
      | rangeStart range == position =
          range : go (rangeEnd range + 1) rest
      | otherwise =
          Range position (rangeStart range - 1) defaultValue
            : range
            : go (rangeEnd range + 1) rest

mergeAdjacent :: (Eq a) => [Range a] -> [Range a]
mergeAdjacent [] = []
mergeAdjacent (first : rest) = reverse (foldl merge [first] rest)
  where
    merge [] range = [range]
    merge (previous : accumulated) range
      | rangeEnd previous + 1 == rangeStart range,
        rangeValue previous == rangeValue range =
          previous {rangeEnd = rangeEnd range} : accumulated
      | otherwise = range : previous : accumulated

fmapRange :: (a -> b) -> Range a -> Range b
fmapRange function range =
  Range
    { rangeStart = rangeStart range,
      rangeEnd = rangeEnd range,
      rangeValue = function (rangeValue range)
    }

parseVersion :: String -> (Int, Int, Int)
parseVersion version =
  case splitOn '.' version of
    [major, minor, patch] -> (read major, read minor, read patch)
    _ -> error ("Unicode version must have MAJOR.MINOR.PATCH form: " <> version)

renderHeader :: String -> (Int, Int, Int) -> String
renderHeader version (major, minor, patch) =
  unlines
    [ "-- DO NOT EDIT: generated by GHC's ucd2haskell with the AIHC backend from Unicode " <> version <> ".",
      "-- Source data: https://www.unicode.org/Public/" <> version <> "/ucd/",
      "",
      "{-# LANGUAGE GHCForeignImportPrim #-}",
      "{-# LANGUAGE MagicHash #-}",
      "",
      "module GHC.Prim.Unicode",
      "  ( charToInt# ,",
      "    generalCategory# ,",
      "    intToChar# ,",
      "    isLowercase# ,",
      "    isUppercase# ,",
      "    unicodeToLower ,",
      "    unicodeToTitle ,",
      "    unicodeToUpper ,",
      "    unicodeVersionMajor# ,",
      "    unicodeVersionMinor# ,",
      "    unicodeVersionPatch# ,",
      "  )",
      "where",
      "",
      "foreign import prim (<#) :: Int# -> Int# -> Int#",
      "",
      "foreign import prim (+#) :: Int# -> Int# -> Int#",
      "",
      "foreign import prim (-#) :: Int# -> Int# -> Int#",
      "",
      "foreign import prim charToInt# :: Char# -> Int#",
      "",
      "foreign import prim intToChar# :: Int# -> Char#",
      "",
      "unicodeVersionMajor# :: Int#",
      "unicodeVersionMajor# = " <> show major <> "#",
      "",
      "unicodeVersionMinor# :: Int#",
      "unicodeVersionMinor# = " <> show minor <> "#",
      "",
      "unicodeVersionPatch# :: Int#",
      "unicodeVersionPatch# = " <> show patch <> "#",
      "",
      "generalCategory# :: Char# -> Int#",
      "generalCategory# value = generalCategoryCode# (charToInt# value)",
      "",
      "isUppercase# :: Char# -> Int#",
      "isUppercase# value = isUppercaseCode# (charToInt# value)",
      "",
      "isLowercase# :: Char# -> Int#",
      "isLowercase# value = isLowercaseCode# (charToInt# value)",
      "",
      "unicodeToUpper :: Char# -> Char#",
      "unicodeToUpper value = intToChar# (toUpperCode# (charToInt# value))",
      "",
      "unicodeToLower :: Char# -> Char#",
      "unicodeToLower value = intToChar# (toLowerCode# (charToInt# value))",
      "",
      "unicodeToTitle :: Char# -> Char#",
      "unicodeToTitle value = intToChar# (toTitleCode# (charToInt# value))",
      ""
    ]

renderLookup :: String -> [Range a] -> (a -> String) -> String
renderLookup name ranges renderValue =
  unlines
    [ name <> " :: Int# -> Int#",
      name <> " n =",
      indent 2 (renderTree ranges renderValue),
      ""
    ]

renderTree :: [Range a] -> (a -> String) -> String
renderTree [] _ = error "cannot render an empty Unicode range tree"
renderTree [range] renderValue = renderValue (rangeValue range)
renderTree ranges renderValue =
  let middle = length ranges `div` 2
   in case splitAt middle ranges of
        (left, right@(firstRight : _)) ->
          unlines
            [ "case n <# " <> show (rangeStart firstRight) <> "# of",
              indent 2 ("0# ->\n" <> indent 2 (renderTree right renderValue)),
              indent 2 ("_ ->\n" <> indent 2 (renderTree left renderValue))
            ]
        _ -> error "cannot split a non-trivial Unicode range tree"

renderBit :: Bool -> String
renderBit False = "0#"
renderBit True = "1#"

renderMapping :: Mapping -> String
renderMapping Identity = "n"
renderMapping (Delta delta)
  | delta < 0 = "n -# " <> show (abs delta) <> "#"
  | otherwise = "n +# " <> show delta <> "#"

indent :: Int -> String -> String
indent amount = intercalate "\n" . map (replicate amount ' ' <>) . lines

splitOn :: Char -> String -> [String]
splitOn delimiter = go
  where
    go value =
      case break (== delimiter) value of
        (field, []) -> [field]
        (field, _ : rest) -> field : go rest
