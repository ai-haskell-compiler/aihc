-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Properties (tests) where

import Control.Applicative
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.Int
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Printf
import Prelude

tests :: TestTree
tests = testGroup "Properties"
    [ testGroup "Decimals"
        [ testProperty "Int"    (\a -> Just (a :: Int)    == readBack a)
        , testProperty "Int8"   (\a -> Just (a :: Int8)   == readBack a)
        , testProperty "Int16"  (\a -> Just (a :: Int16)  == readBack a)
        , testProperty "Int32"  (\a -> Just (a :: Int32)  == readBack a)
        , testProperty "Int64"  (\a -> Just (a :: Int64)  == readBack a)
        , testProperty "Word"   (\a -> Just (a :: Word)   == readBack a)
        , testProperty "Word8 " (\a -> Just (a :: Word8)  == readBack a)
        , testProperty "Word16" (\a -> Just (a :: Word16) == readBack a)
        , testProperty "Word32" (\a -> Just (a :: Word32) == readBack a)
        , testProperty "Word64" (\a -> Just (a :: Word64) == readBack a)
        ]
    , testGroup "Hexadecimals"
        [ testProperty "Int"    (\a -> Just (a :: Hex Int)    == readBackHex a)
        , testProperty "Int8"   (\a -> Just (a :: Hex Int8)   == readBackHex a)
        , testProperty "Int16"  (\a -> Just (a :: Hex Int16)  == readBackHex a)
        , testProperty "Int32"  (\a -> Just (a :: Hex Int32)  == readBackHex a)
        , testProperty "Int64"  (\a -> Just (a :: Hex Int64)  == readBackHex a)
        , testProperty "Word"   (\a -> Just (a :: Hex Word)   == readBackHex a)
        , testProperty "Word8 " (\a -> Just (a :: Hex Word8)  == readBackHex a)
        , testProperty "Word16" (\a -> Just (a :: Hex Word16) == readBackHex a)
        , testProperty "Word32" (\a -> Just (a :: Hex Word32) == readBackHex a)
        , testProperty "Word64" (\a -> Just (a :: Hex Word64) == readBackHex a)
        ]
    , testGroup "Bool"
        [ testProperty "True"  readBackTrue
        , testProperty "False" readBackFalse
        ]
    , testGroup "Double"
        [ testProperty "Double" readBackDouble
        ]
    , testGroup "List"
        [ testProperty "List Int"     (readCSV :: List Int -> Bool)
        , testProperty "List Word"    (readCSV :: List Word -> Bool)
        , testProperty "List Double"  (readCSV :: List Double -> Bool)
        , testProperty "List Bool"    (readCSV :: List Bool -> Bool)
        , testProperty "List Hex"     readHexCSV
        , testProperty "Error"     readDoubleCSVAsInt
        ]
    ]

readBack :: (Show a, FromByteString a) => a -> Maybe a
readBack = fromByteString . pack . show

readBackDouble :: Double -> Bool
readBackDouble d = Just d == (fromByteString . pack . show $ d)

readBackHex :: (PrintfArg i, Show i, FromByteString i, Integral i) => i -> Maybe i
readBackHex = fromByteString . pack . printf "+0x%x"

readBackTrue :: Property
readBackTrue = forAll (elements ["True", "true"]) $
    fromMaybe False . fromByteString

readBackFalse :: Property
readBackFalse = forAll (elements ["False", "false"]) $
    fromMaybe False . fmap not . fromByteString

readCSV :: (Eq a, Show a, FromByteString a) => List a -> Bool
readCSV lst = Just lst == fromByteString (pack (csv lst))
  where
    csv = intercalate "," . map show . fromList

readHexCSV :: List HexStr -> Bool
readHexCSV lst =
    let x = fromByteString (pack (csv lst))
        y = map (snd . hex) (fromList lst)
    in x == Just (List y)
  where
    csv = intercalate "," . map (fst . hex) . fromList

readDoubleCSVAsInt :: List Double -> Bool
readDoubleCSVAsInt (List []) = True
readDoubleCSVAsInt lst       = Nothing == (fromByteString (pack (csv lst)) :: Maybe (List Int))
  where
    csv = intercalate "," . map show . fromList

newtype HexStr = HexStr
    { hex :: (String, Hex Int)
    } deriving (Show)

instance Arbitrary HexStr where
    arbitrary = do
        i <- arbitrary
        x <- elements ['x', 'X']
        return $ HexStr (printf ('+':'0':x:"%x") i, i)

instance Arbitrary a => Arbitrary (Hex a) where
    arbitrary = Hex <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = List <$> arbitrary

