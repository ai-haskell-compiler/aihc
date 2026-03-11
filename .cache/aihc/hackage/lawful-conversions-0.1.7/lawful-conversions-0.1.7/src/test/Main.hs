module Main where

import qualified Data.ByteString.Builder as Data.ByteString.Builder
import qualified Data.ByteString.Lazy as Data.ByteString.Lazy
import qualified Data.ByteString.Short as Data.ByteString.Short
import qualified Data.Primitive.ByteArray as Data.Primitive.ByteArray
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Data.Text.Lazy.Builder as Data.Text.Lazy.Builder
import LawfulConversions
import Rebase.Prelude
import Test.ExtraInstances ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup "All" $
    [ testIs @[Word8] @ByteString Proxy Proxy,
      testIs @[Word8] @Data.ByteString.Lazy.ByteString Proxy Proxy,
      testIs @[Word8] @Data.ByteString.Short.ShortByteString Proxy Proxy,
      testIs @[Word8] @Data.ByteString.Builder.Builder Proxy Proxy,
      testIs @[Word8] @Data.Primitive.ByteArray.ByteArray Proxy Proxy,
      testIs @[Word8] @[Word8] Proxy Proxy,
      testIs @[Word8] @(Vector Word8) Proxy Proxy,
      testIs @[Word8] @(Seq Word8) Proxy Proxy,
      testIs @Text @Text Proxy Proxy,
      testIs @Text @Data.Text.Lazy.Text Proxy Proxy,
      testIs @Text @Data.Text.Lazy.Builder.Builder Proxy Proxy,
      testIs @Data.Text.Lazy.Text @Data.Text.Lazy.Text Proxy Proxy,
      testIs @Data.Text.Lazy.Text @Text Proxy Proxy,
      testIs @Data.Text.Lazy.Text @Data.Text.Lazy.Builder.Builder Proxy Proxy,
      testIs @Data.Text.Lazy.Builder.Builder @Data.Text.Lazy.Builder.Builder Proxy Proxy,
      testIs @Data.Text.Lazy.Builder.Builder @Text Proxy Proxy,
      testIs @Data.Text.Lazy.Builder.Builder @Data.Text.Lazy.Text Proxy Proxy,
      testIs @ByteString @ByteString Proxy Proxy,
      testIs @ByteString @[Word8] Proxy Proxy,
      testIs @ByteString @Data.ByteString.Lazy.ByteString Proxy Proxy,
      testIs @ByteString @Data.ByteString.Short.ShortByteString Proxy Proxy,
      testIs @ByteString @Data.ByteString.Builder.Builder Proxy Proxy,
      testIs @ByteString @Data.Primitive.ByteArray.ByteArray Proxy Proxy,
      testIs @Data.ByteString.Lazy.ByteString @Data.ByteString.Lazy.ByteString Proxy Proxy,
      testIs @Data.ByteString.Lazy.ByteString @[Word8] Proxy Proxy,
      testIs @Data.ByteString.Lazy.ByteString @ByteString Proxy Proxy,
      testIs @Data.ByteString.Lazy.ByteString @Data.ByteString.Short.ShortByteString Proxy Proxy,
      testIs @Data.ByteString.Lazy.ByteString @Data.ByteString.Builder.Builder Proxy Proxy,
      testIs @Data.ByteString.Lazy.ByteString @Data.Primitive.ByteArray.ByteArray Proxy Proxy,
      testIs @Data.ByteString.Short.ShortByteString @Data.ByteString.Short.ShortByteString Proxy Proxy,
      testIs @Data.ByteString.Short.ShortByteString @[Word8] Proxy Proxy,
      testIs @Data.ByteString.Short.ShortByteString @ByteString Proxy Proxy,
      testIs @Data.ByteString.Short.ShortByteString @Data.ByteString.Lazy.ByteString Proxy Proxy,
      testIs @Data.ByteString.Short.ShortByteString @Data.ByteString.Builder.Builder Proxy Proxy,
      testIs @Data.ByteString.Short.ShortByteString @Data.Primitive.ByteArray.ByteArray Proxy Proxy,
      testIs @Data.ByteString.Builder.Builder @Data.ByteString.Builder.Builder Proxy Proxy,
      testIs @Data.ByteString.Builder.Builder @[Word8] Proxy Proxy,
      testIs @Data.ByteString.Builder.Builder @ByteString Proxy Proxy,
      testIs @Data.ByteString.Builder.Builder @Data.ByteString.Lazy.ByteString Proxy Proxy,
      testIs @Data.ByteString.Builder.Builder @Data.ByteString.Short.ShortByteString Proxy Proxy,
      testIs @Data.ByteString.Builder.Builder @Data.Primitive.ByteArray.ByteArray Proxy Proxy,
      testIs @Data.Primitive.ByteArray.ByteArray @Data.Primitive.ByteArray.ByteArray Proxy Proxy,
      testIs @Data.Primitive.ByteArray.ByteArray @[Word8] Proxy Proxy,
      testIs @Data.Primitive.ByteArray.ByteArray @Data.ByteString.Short.ShortByteString Proxy Proxy,
      testIs @Data.Primitive.ByteArray.ByteArray @ByteString Proxy Proxy,
      testIs @Data.Primitive.ByteArray.ByteArray @Data.ByteString.Lazy.ByteString Proxy Proxy,
      testIs @Data.Primitive.ByteArray.ByteArray @Data.ByteString.Builder.Builder Proxy Proxy,
      testIs @(Vector Word8) @(Vector Word8) Proxy Proxy,
      testIs @(Vector Word8) @[Word8] Proxy Proxy,
      testIs @(Vector Word8) @(Seq Word8) Proxy Proxy,
      testIs @(Seq Word8) @(Seq Word8) Proxy Proxy,
      testIs @(Seq Word8) @[Word8] Proxy Proxy,
      testIs @(Seq Word8) @(Vector Word8) Proxy Proxy,
      testIs @(Set Word8) @(Set Word8) Proxy Proxy,
      testIs @(Set Int) @IntSet Proxy Proxy,
      testIs @IntSet @IntSet Proxy Proxy,
      testIs @IntSet @(Set Int) Proxy Proxy,
      testIs @(Map Word8 Word8) @(Map Word8 Word8) Proxy Proxy,
      testIs @(Map Int Word8) @(IntMap Word8) Proxy Proxy,
      testIs @(IntMap Word8) @(IntMap Word8) Proxy Proxy,
      testIs @(IntMap Word8) @(Map Int Word8) Proxy Proxy,
      testIs @(Maybe Word8) @(Maybe Word8) Proxy Proxy,
      testIs @(Either Word8 Word8) @(Either Word8 Word8) Proxy Proxy,
      testIs @(First Word8) @(First Word8) Proxy Proxy,
      testIs @(Last Word8) @(Last Word8) Proxy Proxy,
      testIs @(Product Word8) @(Product Word8) Proxy Proxy,
      testIs @(Sum Word8) @(Sum Word8) Proxy Proxy,
      testIs @Bool @Bool Proxy Proxy,
      testIs @Char @Char Proxy Proxy,
      testIs @Double @Double Proxy Proxy,
      testIs @Float @Float Proxy Proxy,
      testIs @Int @Int Proxy Proxy,
      testIs @Int @Word Proxy Proxy,
      testIs @Int16 @Int16 Proxy Proxy,
      testIs @Int16 @Word16 Proxy Proxy,
      testIs @Int32 @Int32 Proxy Proxy,
      testIs @Int32 @Word32 Proxy Proxy,
      testIs @Int64 @Int64 Proxy Proxy,
      testIs @Int64 @Word64 Proxy Proxy,
      testIs @Int8 @Int8 Proxy Proxy,
      testIs @Int8 @Word8 Proxy Proxy,
      testIs @Integer @Integer Proxy Proxy,
      testIs @Rational @Rational Proxy Proxy,
      testIs @Word @Int Proxy Proxy,
      testIs @Word @Word Proxy Proxy,
      testIs @Word16 @Int16 Proxy Proxy,
      testIs @Word16 @Word16 Proxy Proxy,
      testIs @Word32 @Int32 Proxy Proxy,
      testIs @Word32 @Word32 Proxy Proxy,
      testIs @Word64 @Int64 Proxy Proxy,
      testIs @Word64 @Word64 Proxy Proxy,
      testIs @Word8 @Int8 Proxy Proxy,
      testIs @Word8 @Word8 Proxy Proxy,
      testIsSome @Data.Text.Lazy.Text @UTCTime Proxy Proxy,
      testIsSome @Data.Text.Lazy.Builder.Builder @UTCTime Proxy Proxy,
      testIsSome @String @UTCTime Proxy Proxy,
      testIsSome @Text @UTCTime Proxy Proxy,
      testIsSome @Data.Text.Lazy.Text @Day Proxy Proxy,
      testIsSome @Data.Text.Lazy.Builder.Builder @Day Proxy Proxy,
      testIsSome @String @Day Proxy Proxy,
      testIsSome @Text @Day Proxy Proxy
    ]

testIs :: (Is a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b, Typeable a, Typeable b) => Proxy a -> Proxy b -> TestTree
testIs superp subp =
  isProperties superp subp
    & fmap (uncurry testProperty)
    & testGroup groupName
  where
    groupName =
      mconcat
        [ show (typeOf (asProxyTypeOf undefined superp)),
          "/",
          show (typeOf (asProxyTypeOf undefined subp))
        ]

testIsSome :: (IsSome a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b, Typeable a, Typeable b) => Proxy a -> Proxy b -> TestTree
testIsSome superp subp =
  isSomeProperties superp subp
    & fmap (uncurry testProperty)
    & testGroup groupName
  where
    groupName =
      mconcat
        [ show (typeOf (asProxyTypeOf undefined superp)),
          "/",
          show (typeOf (asProxyTypeOf undefined subp))
        ]
