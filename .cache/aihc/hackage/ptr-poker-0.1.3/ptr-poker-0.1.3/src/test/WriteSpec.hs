{-# OPTIONS_GHC -Wno-missing-signatures #-}

module WriteSpec (spec) where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Char8 as Char8ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import IsomorphismClass
import qualified Numeric.Limits as NumericLimits
import qualified PtrPoker.Write as Write
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (choose)

spec :: Spec
spec = do
  describe "Write" $ do
    describe "wordAsciiDec" $ do
      it "writes Word in ASCII decimal format correctly"
        $ property
        $ \a ->
          let string = Char8ByteString.unpack (Write.toByteString (Write.wordAsciiDec a))
           in read string === a

    describe "intAsciiDec" $ do
      it "writes Int in ASCII decimal format correctly"
        $ property
        $ \a ->
          let string = Char8ByteString.unpack (Write.toByteString (Write.intAsciiDec a))
           in read string === a

    describe "doubleAsciiDec" $ do
      it "writes Double in ASCII decimal format correctly"
        $ property
        $ forAll realFloatGen
        $ \a ->
          let string = Char8ByteString.unpack (Write.toByteString (Write.doubleAsciiDec a))
           in if isNaN a
                then string === "NaN"
                else read string === a

    describe "zeroNonRealDoubleAsciiDec" $ do
      it "writes real Double in ASCII decimal format correctly"
        $ property
        $ forAll realRealFloatGen
        $ \a ->
          let string = Char8ByteString.unpack (Write.toByteString (Write.zeroNonRealDoubleAsciiDec a))
           in read string === a

      it "writes non-real Double as 0"
        $ property
        $ forAll nonRealRealFloatGen
        $ \a ->
          let string = Char8ByteString.unpack (Write.toByteString (Write.zeroNonRealDoubleAsciiDec a))
           in read @Integer string === 0

    describe "textUtf8" $ do
      it "writes ASCII text correctly"
        $ property
        $ forAll (getNonEmpty <$> (arbitrary `suchThat` (all isAsciiChar . getNonEmpty)))
        $ \str ->
          let text = Text.pack str
           in Write.toByteString (Write.textUtf8 text) === TextEncoding.encodeUtf8 text

      it "writes UTF-8 text correctly"
        $ property
        $ \(NonEmpty str) ->
          let text = Text.pack str
           in Write.toByteString (Write.textUtf8 text) === TextEncoding.encodeUtf8 text

    describe "word8" $ do
      it "writes Word8 correctly"
        $ property
        $ \a -> Write.toByteString (Write.word8 a) === to (ByteStringBuilder.word8 a)

    describe "lWord16" $ do
      it "writes Word16 in little-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.lWord16 a) === to (ByteStringBuilder.word16LE a)

    describe "bWord16" $ do
      it "writes Word16 in big-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.bWord16 a) === to (ByteStringBuilder.word16BE a)

    describe "lWord32" $ do
      it "writes Word32 in little-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.lWord32 a) === to (ByteStringBuilder.word32LE a)

    describe "bWord32" $ do
      it "writes Word32 in big-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.bWord32 a) === to (ByteStringBuilder.word32BE a)

    describe "lWord64" $ do
      it "writes Word64 in little-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.lWord64 a) === to (ByteStringBuilder.word64LE a)

    describe "bWord64" $ do
      it "writes Word64 in big-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.bWord64 a) === to (ByteStringBuilder.word64BE a)

    describe "lInt16" $ do
      it "writes Int16 in little-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.lInt16 a) === to (ByteStringBuilder.int16LE a)

    describe "bInt16" $ do
      it "writes Int16 in big-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.bInt16 a) === to (ByteStringBuilder.int16BE a)

    describe "lInt32" $ do
      it "writes Int32 in little-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.lInt32 a) === to (ByteStringBuilder.int32LE a)

    describe "bInt32" $ do
      it "writes Int32 in big-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.bInt32 a) === to (ByteStringBuilder.int32BE a)

    describe "lInt64" $ do
      it "writes Int64 in little-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.lInt64 a) === to (ByteStringBuilder.int64LE a)

    describe "bInt64" $ do
      it "writes Int64 in big-endian format correctly"
        $ property
        $ \a -> Write.toByteString (Write.bInt64 a) === to (ByteStringBuilder.int64BE a)

-- * Generators

realFloatGen :: Gen Double
realFloatGen =
  frequency
    [ (99, realRealFloatGen),
      (1, nonRealRealFloatGen)
    ]

nonRealRealFloatGen :: Gen Double
nonRealRealFloatGen =
  elements [0 / 0, 1 / 0, (-1) / 0, -0.0]

realRealFloatGen :: Gen Double
realRealFloatGen =
  frequency
    [ (50, fullRangeExponentialRealFloatGen),
      (50, simpleZeroToOneRealFloatGen)
    ]

fullRangeExponentialRealFloatGen :: Gen Double
fullRangeExponentialRealFloatGen =
  choose (NumericLimits.minValue, NumericLimits.maxValue)

simpleZeroToOneRealFloatGen :: Gen Double
simpleZeroToOneRealFloatGen = do
  int <- choose (0 :: Int, 999999)
  return (read ("0." <> show int))

isAsciiChar :: Char -> Bool
isAsciiChar c = c <= '\127'
