module Main where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Short as ByteStringShort
import qualified Data.Primitive.ByteArray as PrimitiveByteArray
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import IsomorphismClass
import Rebase.Prelude
import Test.ExtraInstances ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup "All" $
    [ testPair @[Word8] @ByteString Proxy Proxy,
      testPair @[Word8] @ByteStringLazy.ByteString Proxy Proxy,
      testPair @[Word8] @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @[Word8] @ByteStringBuilder.Builder Proxy Proxy,
      testPair @[Word8] @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @[Word8] @[Word8] Proxy Proxy,
      testPair @[Word8] @(Vector Word8) Proxy Proxy,
      testPair @[Word8] @(Seq Word8) Proxy Proxy,
      testPair @Text @Text Proxy Proxy,
      testPair @Text @TextLazy.Text Proxy Proxy,
      testPair @Text @TextLazyBuilder.Builder Proxy Proxy,
      testPair @TextLazy.Text @TextLazy.Text Proxy Proxy,
      testPair @TextLazy.Text @Text Proxy Proxy,
      testPair @TextLazy.Text @TextLazyBuilder.Builder Proxy Proxy,
      testPair @TextLazyBuilder.Builder @TextLazyBuilder.Builder Proxy Proxy,
      testPair @TextLazyBuilder.Builder @Text Proxy Proxy,
      testPair @TextLazyBuilder.Builder @TextLazy.Text Proxy Proxy,
      testPair @ByteString @ByteString Proxy Proxy,
      testPair @ByteString @[Word8] Proxy Proxy,
      testPair @ByteString @ByteStringLazy.ByteString Proxy Proxy,
      testPair @ByteString @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @ByteString @ByteStringBuilder.Builder Proxy Proxy,
      testPair @ByteString @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @ByteStringLazy.ByteString @ByteStringLazy.ByteString Proxy Proxy,
      testPair @ByteStringLazy.ByteString @[Word8] Proxy Proxy,
      testPair @ByteStringLazy.ByteString @ByteString Proxy Proxy,
      testPair @ByteStringLazy.ByteString @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @ByteStringLazy.ByteString @ByteStringBuilder.Builder Proxy Proxy,
      testPair @ByteStringLazy.ByteString @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @[Word8] Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @ByteString Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @ByteStringLazy.ByteString Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @ByteStringBuilder.Builder Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @ByteStringBuilder.Builder @ByteStringBuilder.Builder Proxy Proxy,
      testPair @ByteStringBuilder.Builder @[Word8] Proxy Proxy,
      testPair @ByteStringBuilder.Builder @ByteString Proxy Proxy,
      testPair @ByteStringBuilder.Builder @ByteStringLazy.ByteString Proxy Proxy,
      testPair @ByteStringBuilder.Builder @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @ByteStringBuilder.Builder @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @[Word8] Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @ByteString Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @ByteStringLazy.ByteString Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @ByteStringBuilder.Builder Proxy Proxy,
      testPair @(Vector Word8) @(Vector Word8) Proxy Proxy,
      testPair @(Vector Word8) @[Word8] Proxy Proxy,
      testPair @(Vector Word8) @(Seq Word8) Proxy Proxy,
      testPair @(Seq Word8) @(Seq Word8) Proxy Proxy,
      testPair @(Seq Word8) @[Word8] Proxy Proxy,
      testPair @(Seq Word8) @(Vector Word8) Proxy Proxy,
      testPair @(Set Word8) @(Set Word8) Proxy Proxy,
      testPair @(Set Int) @IntSet Proxy Proxy,
      testPair @IntSet @IntSet Proxy Proxy,
      testPair @IntSet @(Set Int) Proxy Proxy,
      testPair @(Map Word8 Word8) @(Map Word8 Word8) Proxy Proxy,
      testPair @(Map Int Word8) @(IntMap Word8) Proxy Proxy,
      testPair @(IntMap Word8) @(IntMap Word8) Proxy Proxy,
      testPair @(IntMap Word8) @(Map Int Word8) Proxy Proxy,
      testPair @(Maybe Word8) @(Maybe Word8) Proxy Proxy,
      testPair @(Either Word8 Word8) @(Either Word8 Word8) Proxy Proxy,
      testPair @(First Word8) @(First Word8) Proxy Proxy,
      testPair @(Last Word8) @(Last Word8) Proxy Proxy,
      testPair @(Product Word8) @(Product Word8) Proxy Proxy,
      testPair @(Sum Word8) @(Sum Word8) Proxy Proxy,
      testPair @Bool @Bool Proxy Proxy,
      testPair @Char @Char Proxy Proxy,
      testPair @Double @Double Proxy Proxy,
      testPair @Float @Float Proxy Proxy,
      testPair @Int @Int Proxy Proxy,
      testPair @Int @Word Proxy Proxy,
      testPair @Int16 @Int16 Proxy Proxy,
      testPair @Int16 @Word16 Proxy Proxy,
      testPair @Int32 @Int32 Proxy Proxy,
      testPair @Int32 @Word32 Proxy Proxy,
      testPair @Int64 @Int64 Proxy Proxy,
      testPair @Int64 @Word64 Proxy Proxy,
      testPair @Int8 @Int8 Proxy Proxy,
      testPair @Int8 @Word8 Proxy Proxy,
      testPair @Integer @Integer Proxy Proxy,
      testPair @Rational @Rational Proxy Proxy,
      testPair @Word @Int Proxy Proxy,
      testPair @Word @Word Proxy Proxy,
      testPair @Word16 @Int16 Proxy Proxy,
      testPair @Word16 @Word16 Proxy Proxy,
      testPair @Word32 @Int32 Proxy Proxy,
      testPair @Word32 @Word32 Proxy Proxy,
      testPair @Word64 @Int64 Proxy Proxy,
      testPair @Word64 @Word64 Proxy Proxy,
      testPair @Word8 @Int8 Proxy Proxy,
      testPair @Word8 @Word8 Proxy Proxy,
      testPair @DiffTime @NominalDiffTime Proxy Proxy,
      testPair @DiffTime @Pico Proxy Proxy,
      testPair @NominalDiffTime @Pico Proxy Proxy
    ]

testPair :: (IsomorphicTo a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b, Typeable a, Typeable b) => Proxy a -> Proxy b -> TestTree
testPair superp subp =
  isomorphicToProperties superp subp
    & fmap (uncurry testProperty)
    & testGroup groupName
  where
    groupName =
      mconcat
        [ show (typeOf (asProxyTypeOf undefined superp)),
          "/",
          show (typeOf (asProxyTypeOf undefined subp))
        ]
