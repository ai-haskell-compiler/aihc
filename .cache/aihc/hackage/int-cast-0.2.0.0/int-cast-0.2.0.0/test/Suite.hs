module Main (main) where

import Data.Bits
import Data.Word

import Data.Int

-- import Test.Framework (defaultMain, testGroup, testCase)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
-- import Test.HUnit

import Data.IntCast (intCastMaybe)

-- test casting from type a to b (2nd argument is used as type-index only)
-- testIntCastMaybe :: a -> b -> Bool
testIntCastMaybe :: (Bits a, Integral a, Bits b, Integral b) => a -> b -> Bool
testIntCastMaybe x _y = refRes == iutRes
  where
    refRes = intCastMaybeRef x `asTypeOf` (Just _y)
    iutRes = intCastMaybe    x `asTypeOf` (Just _y)

intCastMaybeRef :: (Integral a1, Integral a) => a -> Maybe a1
intCastMaybeRef x
  | toInteger x == toInteger y = Just y
  | otherwise                  = Nothing
  where
    y = fromIntegral x

{- list generated with

let intTypes = words "Int Int8 Int16 Int32 Int64 Word Word8 Word16 Word32 Word64 Integer"
putStrLn $
  concat [ "    [ "
         , intercalate "\n    , "
           [ concat [ "testProperty \"", t1, "->", t2, "\" (testIntCastMaybe :: ", t1, " -> ", t2, "-> Bool)"]
           | t1 <- types, t2 <- types
           ]
         , "\n    ]"
         ]

-}
testGrp_intCastMaybe :: Test
testGrp_intCastMaybe = testGroup "intCastMaybe"
    [ testProperty "Int->Int"         (testIntCastMaybe :: Int -> Int-> Bool)
    , testProperty "Int->Int8"        (testIntCastMaybe :: Int -> Int8-> Bool)
    , testProperty "Int->Int16"       (testIntCastMaybe :: Int -> Int16-> Bool)
    , testProperty "Int->Int32"       (testIntCastMaybe :: Int -> Int32-> Bool)
    , testProperty "Int->Int64"       (testIntCastMaybe :: Int -> Int64-> Bool)
    , testProperty "Int->Word"        (testIntCastMaybe :: Int -> Word-> Bool)
    , testProperty "Int->Word8"       (testIntCastMaybe :: Int -> Word8-> Bool)
    , testProperty "Int->Word16"      (testIntCastMaybe :: Int -> Word16-> Bool)
    , testProperty "Int->Word32"      (testIntCastMaybe :: Int -> Word32-> Bool)
    , testProperty "Int->Word64"      (testIntCastMaybe :: Int -> Word64-> Bool)
    , testProperty "Int->Integer"     (testIntCastMaybe :: Int -> Integer-> Bool)
    , testProperty "Int8->Int"        (testIntCastMaybe :: Int8 -> Int-> Bool)
    , testProperty "Int8->Int8"       (testIntCastMaybe :: Int8 -> Int8-> Bool)
    , testProperty "Int8->Int16"      (testIntCastMaybe :: Int8 -> Int16-> Bool)
    , testProperty "Int8->Int32"      (testIntCastMaybe :: Int8 -> Int32-> Bool)
    , testProperty "Int8->Int64"      (testIntCastMaybe :: Int8 -> Int64-> Bool)
    , testProperty "Int8->Word"       (testIntCastMaybe :: Int8 -> Word-> Bool)
    , testProperty "Int8->Word8"      (testIntCastMaybe :: Int8 -> Word8-> Bool)
    , testProperty "Int8->Word16"     (testIntCastMaybe :: Int8 -> Word16-> Bool)
    , testProperty "Int8->Word32"     (testIntCastMaybe :: Int8 -> Word32-> Bool)
    , testProperty "Int8->Word64"     (testIntCastMaybe :: Int8 -> Word64-> Bool)
    , testProperty "Int8->Integer"    (testIntCastMaybe :: Int8 -> Integer-> Bool)
    , testProperty "Int16->Int"       (testIntCastMaybe :: Int16 -> Int-> Bool)
    , testProperty "Int16->Int8"      (testIntCastMaybe :: Int16 -> Int8-> Bool)
    , testProperty "Int16->Int16"     (testIntCastMaybe :: Int16 -> Int16-> Bool)
    , testProperty "Int16->Int32"     (testIntCastMaybe :: Int16 -> Int32-> Bool)
    , testProperty "Int16->Int64"     (testIntCastMaybe :: Int16 -> Int64-> Bool)
    , testProperty "Int16->Word"      (testIntCastMaybe :: Int16 -> Word-> Bool)
    , testProperty "Int16->Word8"     (testIntCastMaybe :: Int16 -> Word8-> Bool)
    , testProperty "Int16->Word16"    (testIntCastMaybe :: Int16 -> Word16-> Bool)
    , testProperty "Int16->Word32"    (testIntCastMaybe :: Int16 -> Word32-> Bool)
    , testProperty "Int16->Word64"    (testIntCastMaybe :: Int16 -> Word64-> Bool)
    , testProperty "Int16->Integer"   (testIntCastMaybe :: Int16 -> Integer-> Bool)
    , testProperty "Int32->Int"       (testIntCastMaybe :: Int32 -> Int-> Bool)
    , testProperty "Int32->Int8"      (testIntCastMaybe :: Int32 -> Int8-> Bool)
    , testProperty "Int32->Int16"     (testIntCastMaybe :: Int32 -> Int16-> Bool)
    , testProperty "Int32->Int32"     (testIntCastMaybe :: Int32 -> Int32-> Bool)
    , testProperty "Int32->Int64"     (testIntCastMaybe :: Int32 -> Int64-> Bool)
    , testProperty "Int32->Word"      (testIntCastMaybe :: Int32 -> Word-> Bool)
    , testProperty "Int32->Word8"     (testIntCastMaybe :: Int32 -> Word8-> Bool)
    , testProperty "Int32->Word16"    (testIntCastMaybe :: Int32 -> Word16-> Bool)
    , testProperty "Int32->Word32"    (testIntCastMaybe :: Int32 -> Word32-> Bool)
    , testProperty "Int32->Word64"    (testIntCastMaybe :: Int32 -> Word64-> Bool)
    , testProperty "Int32->Integer"   (testIntCastMaybe :: Int32 -> Integer-> Bool)
    , testProperty "Int64->Int"       (testIntCastMaybe :: Int64 -> Int-> Bool)
    , testProperty "Int64->Int8"      (testIntCastMaybe :: Int64 -> Int8-> Bool)
    , testProperty "Int64->Int16"     (testIntCastMaybe :: Int64 -> Int16-> Bool)
    , testProperty "Int64->Int32"     (testIntCastMaybe :: Int64 -> Int32-> Bool)
    , testProperty "Int64->Int64"     (testIntCastMaybe :: Int64 -> Int64-> Bool)
    , testProperty "Int64->Word"      (testIntCastMaybe :: Int64 -> Word-> Bool)
    , testProperty "Int64->Word8"     (testIntCastMaybe :: Int64 -> Word8-> Bool)
    , testProperty "Int64->Word16"    (testIntCastMaybe :: Int64 -> Word16-> Bool)
    , testProperty "Int64->Word32"    (testIntCastMaybe :: Int64 -> Word32-> Bool)
    , testProperty "Int64->Word64"    (testIntCastMaybe :: Int64 -> Word64-> Bool)
    , testProperty "Int64->Integer"   (testIntCastMaybe :: Int64 -> Integer-> Bool)
    , testProperty "Word->Int"        (testIntCastMaybe :: Word -> Int-> Bool)
    , testProperty "Word->Int8"       (testIntCastMaybe :: Word -> Int8-> Bool)
    , testProperty "Word->Int16"      (testIntCastMaybe :: Word -> Int16-> Bool)
    , testProperty "Word->Int32"      (testIntCastMaybe :: Word -> Int32-> Bool)
    , testProperty "Word->Int64"      (testIntCastMaybe :: Word -> Int64-> Bool)
    , testProperty "Word->Word"       (testIntCastMaybe :: Word -> Word-> Bool)
    , testProperty "Word->Word8"      (testIntCastMaybe :: Word -> Word8-> Bool)
    , testProperty "Word->Word16"     (testIntCastMaybe :: Word -> Word16-> Bool)
    , testProperty "Word->Word32"     (testIntCastMaybe :: Word -> Word32-> Bool)
    , testProperty "Word->Word64"     (testIntCastMaybe :: Word -> Word64-> Bool)
    , testProperty "Word->Integer"    (testIntCastMaybe :: Word -> Integer-> Bool)
    , testProperty "Word8->Int"       (testIntCastMaybe :: Word8 -> Int-> Bool)
    , testProperty "Word8->Int8"      (testIntCastMaybe :: Word8 -> Int8-> Bool)
    , testProperty "Word8->Int16"     (testIntCastMaybe :: Word8 -> Int16-> Bool)
    , testProperty "Word8->Int32"     (testIntCastMaybe :: Word8 -> Int32-> Bool)
    , testProperty "Word8->Int64"     (testIntCastMaybe :: Word8 -> Int64-> Bool)
    , testProperty "Word8->Word"      (testIntCastMaybe :: Word8 -> Word-> Bool)
    , testProperty "Word8->Word8"     (testIntCastMaybe :: Word8 -> Word8-> Bool)
    , testProperty "Word8->Word16"    (testIntCastMaybe :: Word8 -> Word16-> Bool)
    , testProperty "Word8->Word32"    (testIntCastMaybe :: Word8 -> Word32-> Bool)
    , testProperty "Word8->Word64"    (testIntCastMaybe :: Word8 -> Word64-> Bool)
    , testProperty "Word8->Integer"   (testIntCastMaybe :: Word8 -> Integer-> Bool)
    , testProperty "Word16->Int"      (testIntCastMaybe :: Word16 -> Int-> Bool)
    , testProperty "Word16->Int8"     (testIntCastMaybe :: Word16 -> Int8-> Bool)
    , testProperty "Word16->Int16"    (testIntCastMaybe :: Word16 -> Int16-> Bool)
    , testProperty "Word16->Int32"    (testIntCastMaybe :: Word16 -> Int32-> Bool)
    , testProperty "Word16->Int64"    (testIntCastMaybe :: Word16 -> Int64-> Bool)
    , testProperty "Word16->Word"     (testIntCastMaybe :: Word16 -> Word-> Bool)
    , testProperty "Word16->Word8"    (testIntCastMaybe :: Word16 -> Word8-> Bool)
    , testProperty "Word16->Word16"   (testIntCastMaybe :: Word16 -> Word16-> Bool)
    , testProperty "Word16->Word32"   (testIntCastMaybe :: Word16 -> Word32-> Bool)
    , testProperty "Word16->Word64"   (testIntCastMaybe :: Word16 -> Word64-> Bool)
    , testProperty "Word16->Integer"  (testIntCastMaybe :: Word16 -> Integer-> Bool)
    , testProperty "Word32->Int"      (testIntCastMaybe :: Word32 -> Int-> Bool)
    , testProperty "Word32->Int8"     (testIntCastMaybe :: Word32 -> Int8-> Bool)
    , testProperty "Word32->Int16"    (testIntCastMaybe :: Word32 -> Int16-> Bool)
    , testProperty "Word32->Int32"    (testIntCastMaybe :: Word32 -> Int32-> Bool)
    , testProperty "Word32->Int64"    (testIntCastMaybe :: Word32 -> Int64-> Bool)
    , testProperty "Word32->Word"     (testIntCastMaybe :: Word32 -> Word-> Bool)
    , testProperty "Word32->Word8"    (testIntCastMaybe :: Word32 -> Word8-> Bool)
    , testProperty "Word32->Word16"   (testIntCastMaybe :: Word32 -> Word16-> Bool)
    , testProperty "Word32->Word32"   (testIntCastMaybe :: Word32 -> Word32-> Bool)
    , testProperty "Word32->Word64"   (testIntCastMaybe :: Word32 -> Word64-> Bool)
    , testProperty "Word32->Integer"  (testIntCastMaybe :: Word32 -> Integer-> Bool)
    , testProperty "Word64->Int"      (testIntCastMaybe :: Word64 -> Int-> Bool)
    , testProperty "Word64->Int8"     (testIntCastMaybe :: Word64 -> Int8-> Bool)
    , testProperty "Word64->Int16"    (testIntCastMaybe :: Word64 -> Int16-> Bool)
    , testProperty "Word64->Int32"    (testIntCastMaybe :: Word64 -> Int32-> Bool)
    , testProperty "Word64->Int64"    (testIntCastMaybe :: Word64 -> Int64-> Bool)
    , testProperty "Word64->Word"     (testIntCastMaybe :: Word64 -> Word-> Bool)
    , testProperty "Word64->Word8"    (testIntCastMaybe :: Word64 -> Word8-> Bool)
    , testProperty "Word64->Word16"   (testIntCastMaybe :: Word64 -> Word16-> Bool)
    , testProperty "Word64->Word32"   (testIntCastMaybe :: Word64 -> Word32-> Bool)
    , testProperty "Word64->Word64"   (testIntCastMaybe :: Word64 -> Word64-> Bool)
    , testProperty "Word64->Integer"  (testIntCastMaybe :: Word64 -> Integer-> Bool)
    , testProperty "Integer->Int"     (testIntCastMaybe :: Integer -> Int-> Bool)
    , testProperty "Integer->Int8"    (testIntCastMaybe :: Integer -> Int8-> Bool)
    , testProperty "Integer->Int16"   (testIntCastMaybe :: Integer -> Int16-> Bool)
    , testProperty "Integer->Int32"   (testIntCastMaybe :: Integer -> Int32-> Bool)
    , testProperty "Integer->Int64"   (testIntCastMaybe :: Integer -> Int64-> Bool)
    , testProperty "Integer->Word"    (testIntCastMaybe :: Integer -> Word-> Bool)
    , testProperty "Integer->Word8"   (testIntCastMaybe :: Integer -> Word8-> Bool)
    , testProperty "Integer->Word16"  (testIntCastMaybe :: Integer -> Word16-> Bool)
    , testProperty "Integer->Word32"  (testIntCastMaybe :: Integer -> Word32-> Bool)
    , testProperty "Integer->Word64"  (testIntCastMaybe :: Integer -> Word64-> Bool)
    , testProperty "Integer->Integer" (testIntCastMaybe :: Integer -> Integer-> Bool)
    ]

main :: IO ()
main = defaultMain [testGrp_intCastMaybe]
