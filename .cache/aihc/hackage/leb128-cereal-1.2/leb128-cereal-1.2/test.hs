{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
import qualified Data.ByteString as B
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Numeric.Natural
import Data.Word
import Data.Int
import Data.Bits
import Data.List
import Data.Typeable

import Data.Serialize.LEB128

newtype LargeInteger = LargeInteger Integer
    deriving (Show, Eq, Ord, Num, Bits, Real, Enum, Integral, SLEB128)
newtype LargeNatural = LargeNatural Natural
    deriving (Show, Eq, Ord, Num, Bits, Real, Enum, Integral, LEB128)

instance Arbitrary LargeNatural where
    arbitrary = LargeNatural . fromWords <$> arbitrary
      where
        fromWords :: [Word8] -> Natural
        fromWords = foldl' go 0
        go :: Natural -> Word8 -> Natural
        go acc w = (acc `shiftL` 8) + fromIntegral w

instance Arbitrary LargeInteger where
    arbitrary = do
       sign <- arbitrary
       LargeNatural n <- arbitrary
       return $ LargeInteger $ (if sign then negate else id) $ fromIntegral n

-- Only change the default (slight hack)
-- Needed to find corner cases involving non-short encodings
moreTests (QuickCheckTests 100) = QuickCheckTests 100000
moreTests (QuickCheckTests n) = QuickCheckTests n

leb128Tests :: forall a. (Typeable a, Arbitrary a, Show a, LEB128 a) => TestTree
leb128Tests = testGroup (show (typeRep (Proxy @a)))
  [ testCase "empty input" $
    fromLEB128 @a B.empty @=? Left "Failed reading: short encoding\nFrom:\tLEB128\n\n"
  , testProperty "roundtrip" $
    \i -> Right i === fromLEB128 (toLEB128 @a i)
  , testProperty "unique rep" $
    \(B.pack -> bs) -> case fromLEB128 @a bs of
      Left _ -> property ()
      Right i -> bs === toLEB128 i
  ]

sleb128Tests :: forall a. (Typeable a, Arbitrary a, Show a, SLEB128 a) => TestTree
sleb128Tests = testGroup (show (typeRep (Proxy @a)))
  [ testCase "empty input" $
    fromSLEB128 @a B.empty @=? Left "Failed reading: short encoding\nFrom:\tSLEB128\n\n"
  , testProperty "roundtrip" $
    \i -> Right i === fromSLEB128 (toSLEB128 @a i)
  , testProperty "unique rep" $
    \(B.pack -> bs) -> case fromSLEB128 @a bs of
      Left _ -> property ()
      Right i -> bs === toSLEB128 i
  ]

main = defaultMain $ adjustOption moreTests $
  testGroup "tests"
  [ leb128Tests @LargeNatural
  , leb128Tests @Word
  , leb128Tests @Word8
  , leb128Tests @Word16
  , leb128Tests @Word32
  , leb128Tests @Word64
  , sleb128Tests @LargeInteger
  , sleb128Tests @Int
  , sleb128Tests @Int8
  , sleb128Tests @Int16
  , sleb128Tests @Int32
  , sleb128Tests @Int64
  ]
