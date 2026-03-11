module TestsMain where

import Text.Convert

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Typeable (Typeable, typeRep)
import Test.Hspec (Spec, describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Arbitrary (Arbitrary)

main :: IO ()
main = hspec $ do
  describe "asString" $ do
    identityProp asString id id "asString"
    weakInverseProp asString id id "BC.pack" BC.pack
    weakInverseProp asString id id "BLC.pack" BLC.pack
    weakInverseProp asString id id "T.pack" T.pack
    weakInverseProp asString id id "TL.pack" TL.pack
    weakInverseProp asString id id "TLB.fromString" TLB.fromString

  describe "asByteString" $ do
    identityProp asByteString BC.pack id "asByteString"
    weakInverseProp asByteString BC.pack id "BC.unpack" BC.unpack
    weakInverseProp asByteString BC.pack id "BLC.fromStrict" BLC.fromStrict
    weakInverseProp asByteString BC.pack id "TE.decodeUtf8With TEE.lenientDecode" (TE.decodeUtf8With TEE.lenientDecode)
    weakInverseProp asByteString BC.pack BB.toLazyByteString "BB.byteString" BB.byteString

  describe "asLazyByteString" $ do
    identityProp asLazyByteString BLC.pack id "asLazyByteString"
    weakInverseProp asLazyByteString BLC.pack id "id" BLC.unpack
    weakInverseProp asLazyByteString BLC.pack id "id" BLC.toStrict
    weakInverseProp asLazyByteString BLC.pack id "TLE.decodeUtf8With TEE.lenientDecode" (TLE.decodeUtf8With TEE.lenientDecode)
    weakInverseProp asLazyByteString BLC.pack BB.toLazyByteString "BB.lazyByteString" BB.lazyByteString

  describe "asText" $ do
    identityProp asText T.pack id "asText"
    weakInverseProp asText T.pack id "T.unpack" T.unpack
    weakInverseProp asText T.pack id "TE.encodeUtf8" TE.encodeUtf8
    weakInverseProp asText T.pack id "TL.fromStrict" TL.fromStrict
    weakInverseProp asText T.pack id "TLB.fromText" TLB.fromText

  describe "asLazyText" $ do
    identityProp asLazyText TL.pack id "asLazyText"
    weakInverseProp asLazyText TL.pack id "TL.unpack" TL.unpack
    weakInverseProp asLazyText TL.pack id "TLE.encodeUtf8" TLE.encodeUtf8
    weakInverseProp asLazyText TL.pack id "TL.toStrict" TL.toStrict
    weakInverseProp asLazyText TL.pack id "TLB.fromLazyText" TLB.fromLazyText

  describe "asTextBuilder" $ do
    identityProp asTextBuilder TLB.fromString id "asTextBuilder"
    weakInverseProp asTextBuilder TLB.fromString id "TLB.toLazyText" TLB.toLazyText

  describe "asByteStringBuilder" $ do
    identityProp asByteStringBuilder BB.stringUtf8 BB.toLazyByteString "asByteStringBuilder"
    weakInverseProp asByteStringBuilder BB.stringUtf8 id "BB.toLazyByteString" BB.toLazyByteString

identityProp :: forall a c t.
  (Arbitrary a, Typeable t, Show a, Show c, Eq c) =>
  (t -> t) -> (a -> t) -> (t -> c) -> String -> Spec
identityProp testFn gen cmp lbl =
  prop (lbl <> " should be the identity on " <> show (typeRep $ Nothing @t)) $
    \x -> (cmp . testFn . gen) x `shouldBe` (cmp . gen) x

weakInverseProp :: forall a c s t.
  (Arbitrary a, Typeable s, Show a, Show c, Eq c) =>
  (s -> t) -> (a -> t) -> (s -> c) -> String -> (t -> s) -> Spec
weakInverseProp testFn gen cmp lbl inv =
  prop (lbl <> " should be a weak inverse through " <> show (typeRep $ Nothing @s)) $
    \x -> (cmp . inv . testFn . inv . gen) x `shouldBe` (cmp . inv . gen) x
