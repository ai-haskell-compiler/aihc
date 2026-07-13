module Main (main) where

import Data.List (isInfixOf)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC
import Ucd2Haskell

main :: IO ()
main =
  defaultMain $
    testGroup
      "ucd2haskell"
      [ testCase "parses categories, ranges, and simple mappings" testParseUnicodeData,
        testCase "parses selected derived properties" testParseDerivedProperties,
        testCase "generates a versioned balanced lookup module" testGenerateModule,
        QC.testProperty "generation is deterministic" $
          QC.once $
            generateModule "17.0.0" sampleUnicodeData sampleDerivedProperties
              == generateModule "17.0.0" sampleUnicodeData sampleDerivedProperties
      ]

testParseUnicodeData :: IO ()
testParseUnicodeData =
  case parseUnicodeData sampleUnicodeData of
    Left err -> fail err
    Right parsed -> do
      assertEqual
        "categories"
        [Range 0x41 0x41 "Lu", Range 0x61 0x61 "Ll", Range 0x3400 0x4dbf "Lo"]
        (unicodeCategories parsed)
      assertEqual "upper mappings" [(0x61, 0x41)] (unicodeUpperMappings parsed)
      assertEqual "lower mappings" [(0x41, 0x61)] (unicodeLowerMappings parsed)
      assertEqual "title mappings" [(0x61, 0x41)] (unicodeTitleMappings parsed)

testParseDerivedProperties :: IO ()
testParseDerivedProperties =
  assertEqual
    "uppercase ranges"
    (Right [Range 0x41 0x5a (), Range 0x2160 0x216f ()])
    (parseDerivedCoreProperties "Uppercase" sampleDerivedProperties)

testGenerateModule :: IO ()
testGenerateModule =
  case generateModule "17.0.0" sampleUnicodeData sampleDerivedProperties of
    Left err -> fail err
    Right generated -> do
      assertBool "records the pinned Unicode version" ("Unicode 17.0.0" `isInfixOf` generated)
      assertBool "exports the category lookup" ("generalCategory#" `isInfixOf` generated)
      assertBool "uses balanced primitive comparisons" ("case n <#" `isInfixOf` generated)
      assertBool "emits simple case mappings" ("toUpperCode#" `isInfixOf` generated)

sampleUnicodeData :: String
sampleUnicodeData =
  unlines
    [ "0041;LATIN CAPITAL LETTER A;Lu;0;L;;;;;N;;;;0061;",
      "0061;LATIN SMALL LETTER A;Ll;0;L;;;;;N;;;0041;;0041",
      "3400;<CJK Ideograph Extension A, First>;Lo;0;L;;;;;N;;;;;",
      "4DBF;<CJK Ideograph Extension A, Last>;Lo;0;L;;;;;N;;;;;"
    ]

sampleDerivedProperties :: String
sampleDerivedProperties =
  unlines
    [ "0041..005A ; Uppercase # Lu [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z",
      "0061..007A ; Lowercase # Ll [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z",
      "2160..216F ; Uppercase # Nl [16] ROMAN NUMERAL ONE..ROMAN NUMERAL ONE THOUSAND"
    ]
