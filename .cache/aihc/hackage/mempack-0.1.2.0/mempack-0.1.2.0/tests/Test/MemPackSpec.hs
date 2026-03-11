{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MemPackSpec (spec) where

import Control.Applicative ((<|>))
import Control.Monad hiding (fail)
import qualified Control.Monad.Fail as F
import Data.Array.Byte (ByteArray)
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Complex
import Data.Either (isLeft)
import Data.Function (fix)
import Data.Int
import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Ratio
import Data.Text (Text)
import Data.Word
import Foreign.Ptr (IntPtr (..), Ptr, intPtrToPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr)
import Numeric.Natural
import System.Random.Stateful
import Test.Common

-- | Generate extrema around boundaries
newtype E a = E {unE :: a}
  deriving (Eq, Ord, Show, Num, Real, Enum, Bounded, Integral)
#if __GLASGOW_HASKELL__ >= 808
  -- We also want to test GND for compiler versions that can handle it
  deriving newtype MemPack
#else
-- Manually defined instance, since ghc-8.6 has issues with deriving MemPack
instance MemPack a => MemPack (E a) where
  typeName = typeName @a
  packedByteCount = packedByteCount . unE
  unpackM = E <$> unpackM
  packM = packM . unE
#endif

instance Random Length

deriving instance Random Tag

instance Uniform Length where
  uniformM = uniformEnumM
instance UniformRange Length where
  uniformRM = uniformEnumRM
#if MIN_VERSION_random(1,3,0)
  isInRange = isInRangeEnum
#endif

instance Arbitrary Length where
  -- Fun fact: `abs minBound == minBound` for Int, so instead of abs we clear top most
  arbitrary = Length . (\x -> x `clearBit` (finiteBitSize x - 1)) <$> arbitrary

deriving instance Arbitrary Tag

instance Arbitrary (Ptr a) where
  arbitrary = intPtrToPtr . IntPtr . unE <$> arbitrary

instance Arbitrary (StablePtr a) where
  arbitrary = castPtrToStablePtr <$> arbitrary

instance Show (StablePtr a) where
  show = show . castStablePtrToPtr

instance (Arbitrary a, Bounded a, Enum a, Random a) => Arbitrary (E a) where
  arbitrary =
    E
      <$> frequency
        [ (25, arbitrary)
        , (25, chooseAny)
        , (25, choose (minBound, iter succ minBound 100)) -- add a 100
        , (25, choose (iter pred maxBound 100, maxBound)) -- subtract a 100
        ]
    where
      iter :: (a -> a) -> a -> Int -> a
      iter f = fix (\loop c i -> if i <= 0 then c else loop (f c) (i - 1))

instance {-# OVERLAPPING #-} Arbitrary (E Integer) where
  arbitrary =
    E
      <$> frequency
        [ (15, arbitrary)
        , (25, chooseAny)
        , (25, choose (fromIntegral (maxBound :: Int), fromIntegral (maxBound :: Word)))
        , (25, choose (negate (fromIntegral (maxBound :: Word)), fromIntegral (minBound :: Int)))
        , (5, fact <$> choose (21, 300))
        , (5, negate . fact <$> choose (21, 300))
        ]
    where

instance {-# OVERLAPPING #-} Arbitrary (E Natural) where
  arbitrary =
    E
      <$> frequency
        [ (15, fromInteger . getNonNegative <$> arbitrary)
        , (25, uniformRM (0, fromIntegral (maxBound :: Word)) QC)
        , (25, uniformRM (fromIntegral (maxBound :: Word), 2 * fromIntegral (maxBound :: Word)) QC)
        , (5, fact . fromInteger <$> choose (21, 300))
        ]

-- factorial for generating some large numbers
fact :: (Ord a, Num a) => a -> a
fact = go 1
  where
    go !acc n
      | n <= 1 = acc
      | otherwise = go (acc * n) (n - 1)

deriving instance Random a => Random (VarLen a)
deriving instance Arbitrary a => Arbitrary (VarLen a)

data Backtrack
  = IntCase Int
  | Word16Case Word16
  deriving (Show, Eq)

instance Arbitrary Backtrack where
  arbitrary =
    oneof [IntCase . unE <$> arbitrary, Word16Case . unE <$> arbitrary]

instance MemPack Backtrack where
  packedByteCount =
    (+ 1) . \case
      IntCase i -> packedByteCount i
      Word16Case i -> packedByteCount i
  packM = \case
    IntCase i -> packM (Tag 0) >> packM i
    Word16Case i -> packM (Tag 1) >> packM i
  unpackM =
    (IntCase <$> unpackCase 0) <|> (Word16Case <$> unpackCase 1)
    where
      unpackCase :: (Buffer b, MemPack a) => Tag -> Unpack b a
      unpackCase t = do
        t' <- unpackM
        unless (t == t') $ F.fail "Tag mismatch"
        unpackM

expectRoundTrip :: forall a. (MemPack a, Eq a, Show a) => a -> Expectation
expectRoundTrip a = do
  unpackError (pack a) `shouldBe` a
  unpackError (packByteString a) `shouldBe` a

expectNotFullyConsumed ::
  forall a. (MemPack a, Show a) => a -> NonEmptyList Word8 -> Expectation
expectNotFullyConsumed a (NonEmpty xs) = do
  let extraByteCount = length xs
      failOnExtra :: (Buffer b, Semigroup b) => b -> b -> Expectation
      failOnExtra buf extra =
        case unpack (buf <> extra) :: Either SomeError a of
          Left e
            | Just err <- fromSomeError e -> do
                notFullyConsumedRead err `shouldBe` bufferByteCount buf
                notFullyConsumedAvailable err `shouldBe` bufferByteCount buf
                  + packedByteCount (Length extraByteCount) -- account for list length
                  + extraByteCount
                notFullyConsumedTypeName err `shouldBe` typeName @a
            | otherwise -> expectationFailure $ "Unexpected failure: " ++ show e
          Right res -> expectationFailure $ "Unexpectedly unpacked: " ++ show res
  failOnExtra (pack a) (pack xs)
  failOnExtra (packByteString a) (packByteString xs)

memPackSpec :: forall a. (MemPack a, Arbitrary a, Eq a, Show a) => Spec
memPackSpec =
  describe (typeName @a) $ do
    prop "RoundTrip" $ expectRoundTrip @a
    describe "Fail on empty" $ do
      let checkRanOutOfBytes someErr
            | Just err <- fromSomeError someErr = do
                ranOutOfBytesRead err `shouldBe` 0
                ranOutOfBytesAvailable err `shouldBe` 0
                ranOutOfBytesRequested err `shouldSatisfy` (> 0)
            | otherwise = expectationFailure $ "Unexpected failure: " ++ show someErr
          -- Check that the only kind of failure we get is RanOutOfBytes
          failOnEmpty emptyBuffer =
            case unpack emptyBuffer :: Either SomeError a of
              Left e
                | Just (ManyErrors errs) <- fromSomeError e -> mapM_ checkRanOutOfBytes errs
                | otherwise -> checkRanOutOfBytes e
              Right res -> expectationFailure $ "Unexpectedly unpacked: " ++ show res
      it "ByteArray" $ failOnEmpty (mempty :: ByteArray)
      it "ByteString" $ failOnEmpty (mempty :: ByteString)
      it "ShortByteString" $ failOnEmpty (mempty :: ShortByteString)
    prop "Fail on too much" $ expectNotFullyConsumed @a

spec :: Spec
spec = do
  prop "RoundTrip" $ expectRoundTrip @()
  memPackSpec @(Ptr ())
  memPackSpec @(StablePtr ())
  memPackSpec @Float
  memPackSpec @Double
  memPackSpec @Bool
  memPackSpec @(E Char)
  memPackSpec @(E Int)
  memPackSpec @(E Int8)
  memPackSpec @(E Int16)
  memPackSpec @(E Int32)
  memPackSpec @(E Int64)
  memPackSpec @(E Word)
  memPackSpec @(E Word8)
  memPackSpec @(E Word16)
  memPackSpec @(E Word32)
  memPackSpec @(E Word64)
  memPackSpec @[E Int]
  memPackSpec @[E Word]
  memPackSpec @(E Int, E Word)
  memPackSpec @(E Int8, E Word8, E Char)
  memPackSpec @(E Int16, E Word16, ByteArray, Double)
  memPackSpec @(E Int32, E Word32, Float, ByteString, Ptr Char)
  memPackSpec @(E Int64, E Word64, Length, VarLen Word, StablePtr Char, [VarLen Word32])
  memPackSpec @(Tag, Int, Int8, Int16, Int32, Int64, Backtrack)
  memPackSpec @(E (VarLen Word))
  memPackSpec @(E (VarLen Word16))
  memPackSpec @(E (VarLen Word32))
  memPackSpec @(E (VarLen Word64))
  memPackSpec @(E Tag)
  memPackSpec @(E Length)
  memPackSpec @(E Integer)
  memPackSpec @(E Natural)
  memPackSpec @(Maybe String)
  memPackSpec @(Either Float Double)
  memPackSpec @(Complex (E Int))
  memPackSpec @(Ratio (E Int))
  memPackSpec @ByteArray
  memPackSpec @ByteString
  memPackSpec @BSL.ByteString
  memPackSpec @ByteArray
  memPackSpec @Text
  memPackSpec @Backtrack
  prop "Out of bound char" $ forAll (choose (0x110000, maxBound :: Word32)) $ \w32 ->
    unpack @Char (pack w32) `shouldSatisfy` isLeft
  prop "Zero denominator" $ \x -> do
    unpack @(Ratio Int8) (pack (x :: Int8, 0 :: Int8)) `shouldSatisfy` isLeft
  prop "Negative length" $ \(xs :: [Int]) isPinned -> do
    let len = packedByteCount (VarLen (maxBound :: Word)) + sum (map packedByteCount xs)
        packerM = packM (VarLen (maxBound :: Word)) >> mapM_ packM xs
    unpack @[Int] (packWithByteArray isPinned "[Int]" len packerM) `shouldSatisfy` isLeft
