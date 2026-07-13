module Ucd2Haskell
  ( Mapping (..),
    Range (..),
    UnicodeData (..),
    generateModule,
    parseDerivedCoreProperties,
    parseUnicodeData,
    writeGeneratedModule,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isSuffixOf, sortOn)
import Numeric (readHex)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data Range a = Range
  { rangeStart :: !Int,
    rangeEnd :: !Int,
    rangeValue :: a
  }
  deriving (Eq, Show)

data Mapping = Identity | Delta !Int
  deriving (Eq, Show)

data UnicodeData = UnicodeData
  { unicodeCategories :: [Range String],
    unicodeUpperMappings :: [(Int, Int)],
    unicodeLowerMappings :: [(Int, Int)],
    unicodeTitleMappings :: [(Int, Int)]
  }
  deriving (Eq, Show)

data PendingRange = PendingRange
  { pendingStart :: !Int,
    pendingCategory :: String
  }

maxCodePoint :: Int
maxCodePoint = 0x10ffff

generateModule :: String -> String -> String -> Either String String
generateModule version unicodeDataText derivedPropertiesText = do
  versionParts <- parseVersion version
  unicodeData <- parseUnicodeData unicodeDataText
  uppercaseRanges <- parseDerivedCoreProperties "Uppercase" derivedPropertiesText
  lowercaseRanges <- parseDerivedCoreProperties "Lowercase" derivedPropertiesText
  categories <- traverse categoryRange (completeRanges "Cn" (unicodeCategories unicodeData))
  let upperMappings = mappingRanges (unicodeUpperMappings unicodeData)
      lowerMappings = mappingRanges (unicodeLowerMappings unicodeData)
      titleMappings = mappingRanges (unicodeTitleMappings unicodeData)
      uppercase = completeRanges False (map (fmapRange (const True)) uppercaseRanges)
      lowercase = completeRanges False (map (fmapRange (const True)) lowercaseRanges)
  pure $
    renderHeader version versionParts
      <> renderLookup "generalCategoryCode#" categories (\tag -> show tag <> "#")
      <> renderLookup "isUppercaseCode#" uppercase renderBit
      <> renderLookup "isLowercaseCode#" lowercase renderBit
      <> renderLookup "toUpperCode#" upperMappings renderMapping
      <> renderLookup "toLowerCode#" lowerMappings renderMapping
      <> renderLookup "toTitleCode#" titleMappings renderMapping

writeGeneratedModule :: FilePath -> String -> IO ()
writeGeneratedModule output generated = do
  createDirectoryIfMissing True (takeDirectory output)
  writeFile output generated

parseUnicodeData :: String -> Either String UnicodeData
parseUnicodeData input = go (1 :: Int) Nothing [] [] [] [] (lines input)
  where
    go _ Nothing categories uppers lowers titles [] =
      Right $
        UnicodeData
          { unicodeCategories = reverse categories,
            unicodeUpperMappings = reverse uppers,
            unicodeLowerMappings = reverse lowers,
            unicodeTitleMappings = reverse titles
          }
    go lineNumber (Just _) _ _ _ _ [] =
      Left ("UnicodeData.txt ended inside a range at line " <> show lineNumber)
    go lineNumber pending categories uppers lowers titles (rawLine : rest)
      | null rawLine = go (lineNumber + 1) pending categories uppers lowers titles rest
      | otherwise = do
          fields <- requireFields lineNumber (splitOn ';' rawLine)
          codePoint <-
            case fields of
              codePointField : _ -> parseHex ("UnicodeData.txt line " <> show lineNumber) codePointField
              [] -> Left ("UnicodeData.txt line " <> show lineNumber <> " is empty")
          let name = fields !! 1
              category = fields !! 2
              upper = parseOptionalMapping codePoint (fields !! 12)
              lower = parseOptionalMapping codePoint (fields !! 13)
              title = parseOptionalMapping codePoint (fields !! 14)
          if ", First>" `isSuffixOf` name
            then case pending of
              Nothing ->
                go
                  (lineNumber + 1)
                  (Just (PendingRange codePoint category))
                  categories
                  uppers
                  lowers
                  titles
                  rest
              Just _ -> Left ("nested UnicodeData.txt range at line " <> show lineNumber)
            else
              if ", Last>" `isSuffixOf` name
                then case pending of
                  Nothing -> Left ("range end without range start at UnicodeData.txt line " <> show lineNumber)
                  Just first
                    | pendingCategory first /= category ->
                        Left ("range category mismatch at UnicodeData.txt line " <> show lineNumber)
                    | otherwise ->
                        go
                          (lineNumber + 1)
                          Nothing
                          (Range (pendingStart first) codePoint category : categories)
                          uppers
                          lowers
                          titles
                          rest
                else case pending of
                  Just _ -> Left ("range start not followed by range end at UnicodeData.txt line " <> show lineNumber)
                  Nothing ->
                    go
                      (lineNumber + 1)
                      Nothing
                      (Range codePoint codePoint category : categories)
                      (maybe uppers (: uppers) upper)
                      (maybe lowers (: lowers) lower)
                      (maybe titles (: titles) title)
                      rest

    requireFields lineNumber fields
      | length fields >= 15 = Right fields
      | otherwise = Left ("UnicodeData.txt line " <> show lineNumber <> " has fewer than 15 fields")

parseDerivedCoreProperties :: String -> String -> Either String [Range ()]
parseDerivedCoreProperties wanted input =
  fmap (sortOn rangeStart . concat) . traverse parseLine $ zip [1 :: Int ..] (lines input)
  where
    parseLine (lineNumber, rawLine) =
      let content = trim (takeWhile (/= '#') rawLine)
       in if null content
            then Right []
            else case splitOn ';' content of
              rawRange : rawProperty : _
                | trim rawProperty == wanted -> do
                    (start, end) <- parseCodePointRange ("DerivedCoreProperties.txt line " <> show lineNumber) (trim rawRange)
                    pure [Range start end ()]
                | otherwise -> Right []
              _ -> Left ("malformed DerivedCoreProperties.txt line " <> show lineNumber)

parseOptionalMapping :: Int -> String -> Maybe (Int, Int)
parseOptionalMapping source raw =
  case parseHexMaybe raw of
    Just target
      | target /= source -> Just (source, target)
    _ -> Nothing

parseVersion :: String -> Either String (Int, Int, Int)
parseVersion version =
  case splitOn '.' version of
    [major, minor, patch] ->
      (,,) <$> parseDecimal major <*> parseDecimal minor <*> parseDecimal patch
    _ -> Left ("Unicode version must have MAJOR.MINOR.PATCH form: " <> version)
  where
    parseDecimal value =
      case reads value of
        [(number, "")] -> Right number
        _ -> Left ("invalid Unicode version component: " <> value)

parseCodePointRange :: String -> String -> Either String (Int, Int)
parseCodePointRange context raw =
  case splitOnString ".." raw of
    [single] -> do
      value <- parseHex context single
      pure (value, value)
    [start, end] -> (,) <$> parseHex context start <*> parseHex context end
    _ -> Left (context <> ": malformed code point range " <> raw)

parseHex :: String -> String -> Either String Int
parseHex context raw =
  maybe (Left (context <> ": invalid hexadecimal value " <> raw)) Right (parseHexMaybe raw)

parseHexMaybe :: String -> Maybe Int
parseHexMaybe raw =
  case readHex raw of
    [(value, "")] -> Just value
    _ -> Nothing

categoryRange :: Range String -> Either String (Range Int)
categoryRange range = do
  tag <- categoryTag (rangeValue range)
  pure (fmapRange (const tag) range)

categoryTag :: String -> Either String Int
categoryTag category =
  case lookup category categoryTags of
    Just tag -> Right tag
    Nothing -> Left ("unknown Unicode general category: " <> category)

categoryTags :: [(String, Int)]
categoryTags =
  zip
    [ "Lu",
      "Ll",
      "Lt",
      "Lm",
      "Lo",
      "Mn",
      "Mc",
      "Me",
      "Nd",
      "Nl",
      "No",
      "Pc",
      "Pd",
      "Ps",
      "Pe",
      "Pi",
      "Pf",
      "Po",
      "Sm",
      "Sc",
      "Sk",
      "So",
      "Zs",
      "Zl",
      "Zp",
      "Cc",
      "Cf",
      "Cs",
      "Co",
      "Cn"
    ]
    [0 ..]

mappingRanges :: [(Int, Int)] -> [Range Mapping]
mappingRanges mappings = completeRanges Identity (compressMappings (sortOn fst mappings))

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
fmapRange f range =
  Range
    { rangeStart = rangeStart range,
      rangeEnd = rangeEnd range,
      rangeValue = f (rangeValue range)
    }

renderHeader :: String -> (Int, Int, Int) -> String
renderHeader version (major, minor, patch) =
  unlines
    [ "-- DO NOT EDIT: generated by tooling/ucd2haskell from Unicode " <> version <> ".",
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
      "foreign import prim charToInt# :: Char -> Int#",
      "",
      "foreign import prim intToChar# :: Int# -> Char",
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
      "generalCategory# :: Char -> Int#",
      "generalCategory# value = generalCategoryCode# (charToInt# value)",
      "",
      "isUppercase# :: Char -> Int#",
      "isUppercase# value = isUppercaseCode# (charToInt# value)",
      "",
      "isLowercase# :: Char -> Int#",
      "isLowercase# value = isLowercaseCode# (charToInt# value)",
      "",
      "unicodeToUpper :: Char -> Char",
      "unicodeToUpper value = intToChar# (toUpperCode# (charToInt# value))",
      "",
      "unicodeToLower :: Char -> Char",
      "unicodeToLower value = intToChar# (toLowerCode# (charToInt# value))",
      "",
      "unicodeToTitle :: Char -> Char",
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

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn delimiter = go
  where
    go value =
      case break (== delimiter) value of
        (field, []) -> [field]
        (field, _ : rest) -> field : go rest

splitOnString :: String -> String -> [String]
splitOnString delimiter value =
  case breakOn delimiter value of
    Nothing -> [value]
    Just (before, after) -> before : splitOnString delimiter after

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go []
  where
    go _ [] = Nothing
    go prefix remaining@(first : rest)
      | needle `prefixOf` remaining = Just (reverse prefix, drop (length needle) remaining)
      | otherwise = go (first : prefix) rest

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (expected : expectedRest) (actual : actualRest) =
  expected == actual && prefixOf expectedRest actualRest
