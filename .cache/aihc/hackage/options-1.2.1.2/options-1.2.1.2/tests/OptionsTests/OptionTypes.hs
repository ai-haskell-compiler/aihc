-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.OptionTypes (suite_OptionTypes) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Word (Word16, Word32, Word64, Word8)
import Options
import Test.Hspec (Expectation, Spec, context, shouldBe, specify)

suite_OptionTypes :: Spec
suite_OptionTypes = context "option-types" do
  test_Bool
  test_String
  test_Int
  test_Int8
  test_Int16
  test_Int32
  test_Int64
  test_Word
  test_Word8
  test_Word16
  test_Word32
  test_Word64
  test_Integer
  test_Float
  test_Double
  test_Maybe
  test_List
  test_Set
  test_Map
  test_Enum

parseValid :: (Show a, Eq a) => OptionType a -> String -> a -> Expectation
parseValid t s expected = shouldBe (optionTypeParse t s) (Right expected)

parseInvalid :: (Show a, Eq a) => OptionType a -> String -> String -> Expectation
parseInvalid t s err = shouldBe (optionTypeParse t s) (Left err)

test_Bool :: Spec
test_Bool = specify "bool" $ do
  parseValid optionType_bool "true" True
  parseValid optionType_bool "false" False
  parseInvalid optionType_bool "" "\"\" is not in {\"true\", \"false\"}."

test_String :: Spec
test_String = specify "string" $ do
  let valid = parseValid optionType_string

  valid "" ""
  valid "a" "a"
  valid "\12354" "\12354"
  valid "\56507" "\56507"
  valid "\61371" "\61371"

test_Int :: Spec
test_Int = specify "int" $ do
  let valid = parseValid optionType_int
  let invalid = parseInvalid optionType_int

  valid "-1" (-1 :: Int)
  valid "1" (1 :: Int)
  invalid "a" "\"a\" is not an integer."

  let pastMin = show (toInteger (minBound :: Int) - 1)
  let pastMax = show (toInteger (maxBound :: Int) + 1)
  let errBounds = " is not within bounds [" ++ show (minBound :: Int) ++ ":" ++ show (maxBound :: Int) ++ "] of type int."

  invalid pastMin (pastMin ++ errBounds)
  valid (show (minBound :: Int)) minBound
  valid (show (maxBound :: Int)) maxBound
  invalid pastMax (pastMax ++ errBounds)

test_Int8 :: Spec
test_Int8 = specify "int8" $ do
  let valid = parseValid optionType_int8
  let invalid = parseInvalid optionType_int8

  valid "-1" (-1 :: Int8)
  valid "1" (1 :: Int8)
  invalid "a" "\"a\" is not an integer."

  let pastMin = show (toInteger (minBound :: Int8) - 1)
  let pastMax = show (toInteger (maxBound :: Int8) + 1)
  invalid pastMin "-129 is not within bounds [-128:127] of type int8."
  valid (show (minBound :: Int8)) minBound
  valid (show (maxBound :: Int8)) maxBound
  invalid pastMax "128 is not within bounds [-128:127] of type int8."

test_Int16 :: Spec
test_Int16 = specify "int16" $ do
  let valid = parseValid optionType_int16
  let invalid = parseInvalid optionType_int16

  valid "-1" (-1 :: Int16)
  valid "1" (1 :: Int16)
  invalid "a" "\"a\" is not an integer."

  let pastMin = show (toInteger (minBound :: Int16) - 1)
  let pastMax = show (toInteger (maxBound :: Int16) + 1)
  invalid pastMin "-32769 is not within bounds [-32768:32767] of type int16."
  valid (show (minBound :: Int16)) minBound
  valid (show (maxBound :: Int16)) maxBound
  invalid pastMax "32768 is not within bounds [-32768:32767] of type int16."

test_Int32 :: Spec
test_Int32 = specify "int32" $ do
  let valid = parseValid optionType_int32
  let invalid = parseInvalid optionType_int32

  valid "-1" (-1 :: Int32)
  valid "1" (1 :: Int32)
  invalid "a" "\"a\" is not an integer."

  let pastMin = show (toInteger (minBound :: Int32) - 1)
  let pastMax = show (toInteger (maxBound :: Int32) + 1)
  invalid pastMin "-2147483649 is not within bounds [-2147483648:2147483647] of type int32."
  valid (show (minBound :: Int32)) minBound
  valid (show (maxBound :: Int32)) maxBound
  invalid pastMax "2147483648 is not within bounds [-2147483648:2147483647] of type int32."

test_Int64 :: Spec
test_Int64 = specify "int64" $ do
  let valid = parseValid optionType_int64
  let invalid = parseInvalid optionType_int64

  valid "-1" (-1 :: Int64)
  valid "1" (1 :: Int64)
  invalid "a" "\"a\" is not an integer."

  let pastMin = show (toInteger (minBound :: Int64) - 1)
  let pastMax = show (toInteger (maxBound :: Int64) + 1)
  invalid pastMin "-9223372036854775809 is not within bounds [-9223372036854775808:9223372036854775807] of type int64."
  valid (show (minBound :: Int64)) minBound
  valid (show (maxBound :: Int64)) maxBound
  invalid pastMax "9223372036854775808 is not within bounds [-9223372036854775808:9223372036854775807] of type int64."

test_Word :: Spec
test_Word = specify "word" $ do
  let valid = parseValid optionType_word
  let invalid = parseInvalid optionType_word

  let pastMax = show (toInteger (maxBound :: Word) + 1)
  let errBounds = " is not within bounds [0:" ++ show (maxBound :: Word) ++ "] of type uint."

  invalid "-1" ("-1" ++ errBounds)
  valid "0" (0 :: Word)
  valid "1" (1 :: Word)
  invalid "a" "\"a\" is not an integer."

  valid (show (maxBound :: Word)) maxBound
  invalid pastMax (pastMax ++ errBounds)

test_Word8 :: Spec
test_Word8 = specify "word8" $ do
  let valid = parseValid optionType_word8
  let invalid = parseInvalid optionType_word8

  invalid "-1" "-1 is not within bounds [0:255] of type uint8."
  valid "0" (0 :: Word8)
  valid "1" (1 :: Word8)
  invalid "a" "\"a\" is not an integer."

  let pastMax = show (toInteger (maxBound :: Word8) + 1)
  valid (show (maxBound :: Word8)) maxBound
  invalid pastMax "256 is not within bounds [0:255] of type uint8."

test_Word16 :: Spec
test_Word16 = specify "word16" $ do
  let valid = parseValid optionType_word16
  let invalid = parseInvalid optionType_word16

  invalid "-1" "-1 is not within bounds [0:65535] of type uint16."
  valid "0" (0 :: Word16)
  valid "1" (1 :: Word16)
  invalid "a" "\"a\" is not an integer."

  let pastMax = show (toInteger (maxBound :: Word16) + 1)
  valid (show (maxBound :: Word16)) maxBound
  invalid pastMax "65536 is not within bounds [0:65535] of type uint16."

test_Word32 :: Spec
test_Word32 = specify "word32" $ do
  let valid = parseValid optionType_word32
  let invalid = parseInvalid optionType_word32

  invalid "-1" "-1 is not within bounds [0:4294967295] of type uint32."
  valid "0" (0 :: Word32)
  valid "1" (1 :: Word32)
  invalid "a" "\"a\" is not an integer."

  let pastMax = show (toInteger (maxBound :: Word32) + 1)
  valid (show (maxBound :: Word32)) maxBound
  invalid pastMax "4294967296 is not within bounds [0:4294967295] of type uint32."

test_Word64 :: Spec
test_Word64 = specify "word64" $ do
  let valid = parseValid optionType_word64
  let invalid = parseInvalid optionType_word64

  invalid "-1" "-1 is not within bounds [0:18446744073709551615] of type uint64."
  valid "0" (0 :: Word64)
  valid "1" (1 :: Word64)
  invalid "a" "\"a\" is not an integer."

  let pastMax = show (toInteger (maxBound :: Word64) + 1)
  valid (show (maxBound :: Word64)) maxBound
  invalid pastMax "18446744073709551616 is not within bounds [0:18446744073709551615] of type uint64."

test_Integer :: Spec
test_Integer = specify "integer" $ do
  let valid = parseValid optionType_integer
  let invalid = parseInvalid optionType_integer

  invalid "" "\"\" is not an integer."
  valid "-1" (-1 :: Integer)
  valid "0" (0 :: Integer)
  valid "1" (1 :: Integer)
  invalid "a" "\"a\" is not an integer."

test_Float :: Spec
test_Float = specify "float" $ do
  let valid = parseValid optionType_float
  let invalid = parseInvalid optionType_float

  valid "-1" (-1 :: Float)
  valid "0" (0 :: Float)
  valid "1" (1 :: Float)
  valid "1.5" (1.5 :: Float)
  valid "3e5" (3e5 :: Float)
  invalid "a" "\"a\" is not a number."

test_Double :: Spec
test_Double = specify "double" $ do
  let valid = parseValid optionType_double
  let invalid = parseInvalid optionType_double

  valid "-1" (-1 :: Double)
  valid "0" (0 :: Double)
  valid "1" (1 :: Double)
  valid "1.5" (1.5 :: Double)
  valid "3e5" (3e5 :: Double)
  invalid "a" "\"a\" is not a number."

test_Maybe :: Spec
test_Maybe = specify "maybe" $ do
  let t = optionType_maybe optionType_int
  let valid = parseValid t
  let invalid = parseInvalid t

  valid "" Nothing
  valid "1" (Just 1)
  invalid "a" "\"a\" is not an integer."

test_List :: Spec
test_List = specify "list" $ do
  let t = optionType_list ',' optionType_int
  let valid = parseValid t
  let invalid = parseInvalid t

  valid "" []
  valid "1" [1]
  valid "1,2,3" [1, 2, 3]
  valid "1,1,2,3" [1, 1, 2, 3]
  invalid "1,a,3" "\"a\" is not an integer."

test_Set :: Spec
test_Set = specify "set" $ do
  let t = optionType_set ',' optionType_int
  let valid = parseValid t
  let invalid = parseInvalid t

  valid "" Set.empty
  valid "1" (Set.fromList [1])
  valid "1,2,3" (Set.fromList [1, 2, 3])
  valid "1,1,2,3" (Set.fromList [1, 2, 3])
  invalid "1,a,3" "\"a\" is not an integer."

test_Map :: Spec
test_Map = specify "map" $ do
  let t = optionType_map ',' '=' optionType_int optionType_int
  let valid = parseValid t
  let invalid = parseInvalid t

  valid "" Map.empty
  valid "1=100" (Map.fromList [(1, 100)])
  valid "1=100,2=200,3=300" (Map.fromList [(1, 100), (2, 200), (3, 300)])
  valid "1=100,2=200,1=300" (Map.fromList [(1, 300), (2, 200)])
  invalid "a=1" "\"a\" is not an integer."
  invalid "1=a" "\"a\" is not an integer."
  invalid "1=" "\"\" is not an integer."
  invalid "1" "Map item \"1\" has no value."

data TestEnum = Enum1 | Enum2 | Enum3
  deriving (Bounded, Enum, Eq, Show)

test_Enum :: Spec
test_Enum = specify "enum" $ do
  let t = optionType_enum "test enum"
  let valid = parseValid t
  let invalid = parseInvalid t

  valid "Enum1" Enum1
  valid "Enum2" Enum2
  invalid "Enum4" "\"Enum4\" is not in {\"Enum1\", \"Enum2\", \"Enum3\"}."
