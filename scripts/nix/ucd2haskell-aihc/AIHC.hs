{-# LANGUAGE ImportQualifiedPost #-}

module UCD2Haskell.AIHC (genAihcModule) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Short qualified as BS
import Data.Char (ord)
import Data.List (sortOn)
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
      classifications =
        combineRanges3
          (\category upper lower -> category + (if upper then 32 else 0) + (if lower then 64 else 0))
          categories
          uppercase
          lowercase
      caseMappings =
        combineRangesWith
          (\upper lower -> (mappingDelta upper, mappingDelta lower))
          upperMappings
          lowerMappings
      titleOverrides =
        combineRangesWith
          (\upper title -> mappingDelta title - mappingDelta upper)
          upperMappings
          titleMappings
   in renderHeader version
        <> renderClassificationTable classifications
        <> renderCaseMappingTable caseMappings
        <> renderTitleOverrideTable titleOverrides
        <> renderLookupHelpers

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

combineRangesWith :: (Eq c) => (a -> b -> c) -> [Range a] -> [Range b] -> [Range c]
combineRangesWith combine left right = mergeAdjacent (go left right)
  where
    go [] [] = []
    go (left : leftRest) (right : rightRest)
      | rangeStart left /= rangeStart right =
          error "cannot combine Unicode ranges with different starts"
      | otherwise =
          Range
            { rangeStart = rangeStart left,
              rangeEnd = end,
              rangeValue = combine (rangeValue left) (rangeValue right)
            }
            : go (advance end left leftRest) (advance end right rightRest)
      where
        end = min (rangeEnd left) (rangeEnd right)
    go _ _ = error "cannot combine incomplete Unicode ranges"

    advance end range rest
      | rangeEnd range == end = rest
      | otherwise = range {rangeStart = end + 1} : rest

combineRanges3 :: (Eq a, Eq b, Eq d) => (a -> b -> c -> d) -> [Range a] -> [Range b] -> [Range c] -> [Range d]
combineRanges3 combine left middle =
  combineRangesWith (uncurry combine) (combineRangesWith (,) left middle)

mappingDelta :: Mapping -> Int
mappingDelta Identity = 0
mappingDelta (Delta delta) = delta

fmapRange :: (a -> b) -> Range a -> Range b
fmapRange function range =
  Range
    { rangeStart = rangeStart range,
      rangeEnd = rangeEnd range,
      rangeValue = function (rangeValue range)
    }

renderHeader :: String -> String
renderHeader version =
  unlines
    [ "-- DO NOT EDIT: generated by GHC's ucd2haskell with the AIHC backend from Unicode " <> version <> ".",
      "-- Source data: https://www.unicode.org/Public/" <> version <> "/ucd/",
      "",
      "{-# LANGUAGE MagicHash #-}",
      "",
      "module GHC.Prim.Unicode",
      "  ( generalCategory# ,",
      "    isLowercase# ,",
      "    isUppercase# ,",
      "    unicodeToLower ,",
      "    unicodeToTitle ,",
      "    unicodeToUpper ,",
      "  )",
      "where",
      "",
      "import GHC.Prim (Addr#, Char#, Int#, and#, chr#, indexWord8OffAddr#, int2Word#, ord#, uncheckedShiftRL#, word2Int#, word8ToWord#, (+#), (-#), (*#), (<#))",
      "",
      "generalCategory# :: Char# -> Int#",
      "generalCategory# value = andInt# (classificationCode# (ord# value)) 31#",
      "",
      "isUppercase# :: Char# -> Int#",
      "isUppercase# value = andInt# (shiftRightInt# (classificationCode# (ord# value)) 5#) 1#",
      "",
      "isLowercase# :: Char# -> Int#",
      "isLowercase# value = andInt# (shiftRightInt# (classificationCode# (ord# value)) 6#) 1#",
      "",
      "unicodeToUpper :: Char# -> Char#",
      "unicodeToUpper value = chr# (applyMapping# 3# (ord# value))",
      "",
      "unicodeToLower :: Char# -> Char#",
      "unicodeToLower value = chr# (applyMapping# 6# (ord# value))",
      "",
      "unicodeToTitle :: Char# -> Char#",
      "unicodeToTitle value = chr# (applyTitleMapping# (ord# value))",
      ""
    ]

renderClassificationTable :: [Range Int] -> String
renderClassificationTable ranges =
  unlines
    [ "-- Each four-byte record contains a 24-bit inclusive range end followed by",
      "-- five category bits and the Uppercase and Lowercase property bits.",
      "classificationCode# :: Int# -> Int#",
      "classificationCode# n =",
      "  case " <> renderAddrLiteral (concatMap encodeRange ranges) <> " of",
      "    table ->",
      "      case lookupRangeIndex# table 4# " <> renderIntHash (length ranges) <> " n of",
      "        index -> indexByte# table (index *# 4# +# 3#)",
      ""
    ]
  where
    encodeRange range = encodeUnsigned24 (rangeEnd range) <> [encodeByte (rangeValue range)]

renderCaseMappingTable :: [Range (Int, Int)] -> String
renderCaseMappingTable ranges =
  unlines
    [ "-- Each nine-byte record contains a 24-bit inclusive range end followed by",
      "-- signed 24-bit uppercase and lowercase code-point deltas.",
      "applyMapping# :: Int# -> Int# -> Int#",
      "applyMapping# fieldOffset n =",
      "  case " <> renderAddrLiteral (concatMap encodeRange ranges) <> " of",
      "    table ->",
      "      case lookupRangeIndex# table 9# " <> renderIntHash (length ranges) <> " n of",
      "        index -> n +# indexSigned24# table (index *# 9# +# fieldOffset)",
      ""
    ]
  where
    encodeRange range =
      let (upper, lower) = rangeValue range
       in encodeUnsigned24 (rangeEnd range) <> encodeSigned24 upper <> encodeSigned24 lower

renderTitleOverrideTable :: [Range Int] -> String
renderTitleOverrideTable ranges =
  unlines
    [ "-- Titlecase normally equals uppercase. This compact table stores the signed",
      "-- difference between its code-point delta and the uppercase delta.",
      "applyTitleMapping# :: Int# -> Int#",
      "applyTitleMapping# n =",
      "  case " <> renderAddrLiteral (concatMap encodeRange ranges) <> " of",
      "    table ->",
      "      case lookupRangeIndex# table 6# " <> renderIntHash (length ranges) <> " n of",
      "        index -> applyMapping# 3# n +# indexSigned24# table (index *# 6# +# 3#)",
      ""
    ]
  where
    encodeRange range = encodeUnsigned24 (rangeEnd range) <> encodeSigned24 (rangeValue range)

renderLookupHelpers :: String
renderLookupHelpers =
  unlines
    [ "lookupRangeIndex# :: Addr# -> Int# -> Int# -> Int# -> Int#",
      "lookupRangeIndex# table recordSize count n = go 0# count",
      "  where",
      "    go low high =",
      "      case low <# high of",
      "        0# -> low",
      "        _ ->",
      "          case word2Int# (uncheckedShiftRL# (int2Word# (low +# high)) 1#) of",
      "            middle ->",
      "              case n <# (indexUnsigned24# table (middle *# recordSize) +# 1#) of",
      "                0# -> go (middle +# 1#) high",
      "                _ -> go low middle",
      "",
      "indexSigned24# :: Addr# -> Int# -> Int#",
      "indexSigned24# table offset =",
      "  case indexUnsigned24# table offset of",
      "    value ->",
      "      case value <# 8388608# of",
      "        0# -> value -# 16777216#",
      "        _ -> value",
      "",
      "indexUnsigned24# :: Addr# -> Int# -> Int#",
      "indexUnsigned24# table offset =",
      "  indexByte# table offset",
      "    +# (indexByte# table (offset +# 1#) *# 256#)",
      "    +# (indexByte# table (offset +# 2#) *# 65536#)",
      "",
      "indexByte# :: Addr# -> Int# -> Int#",
      "indexByte# table index = word2Int# (word8ToWord# (indexWord8OffAddr# table index))",
      "",
      "shiftRightInt# :: Int# -> Int# -> Int#",
      "shiftRightInt# value amount = word2Int# (uncheckedShiftRL# (int2Word# value) amount)",
      "",
      "andInt# :: Int# -> Int# -> Int#",
      "andInt# value mask = word2Int# (and# (int2Word# value) (int2Word# mask))",
      ""
    ]

encodeUnsigned24 :: Int -> [Int]
encodeUnsigned24 value
  | value < 0 || value >= 16777216 = error ("value does not fit in unsigned 24 bits: " <> show value)
  | otherwise = [value `mod` 256, value `div` 256 `mod` 256, value `div` 65536]

encodeSigned24 :: Int -> [Int]
encodeSigned24 value
  | value < -8388608 || value >= 8388608 = error ("value does not fit in signed 24 bits: " <> show value)
  | otherwise = encodeUnsigned24 (value `mod` 16777216)

encodeByte :: Int -> Int
encodeByte value
  | value < 0 || value >= 256 = error ("value does not fit in a byte: " <> show value)
  | otherwise = value

renderAddrLiteral :: [Int] -> String
renderAddrLiteral bytes = "\"" <> concatMap (("\\" <>) . show . encodeByte) bytes <> "\"#"

renderIntHash :: Int -> String
renderIntHash value = show value <> "#"
