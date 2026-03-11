{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.RoundtripSpec where

import Data.Aeson hiding (encode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
import Data.Registry
import Data.Registry.Aeson.Decoder
import Data.Registry.Aeson.Encoder
import Data.Registry.Hedgehog
import Data.Registry.Hedgehog.TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hedgehog.Gen hiding (maybe, print)
import Hedgehog.Range
import Protolude hiding (bool)
import Test.Data.Registry.Aeson.EncoderSpec (checkValue)
import Test.Data.Registry.Aeson.RoundtripData
import Test.Tasty.Hedgehogx hiding (bool, maybe, string)
import Prelude (String)

test_roundtrip = minTestsOk 1000 $
  prop "roundtrip encoders / decoders" $ do
    roundtrip @T0
    roundtrip @T1
    roundtrip @T2
    roundtrip @T3
    roundtrip @T4
    roundtrip @T5
    roundtrip @T6
    roundtrip @T7
    roundtrip @T8
    roundtrip @T9
    roundtrip @T10
    roundtrip @T11
    roundtrip @T12
    roundtrip @T13
    roundtrip @T14
    roundtrip @T15
    roundtrip @T16

test_regressions = test "regressions" $ do
  checkEncodingsWith options1 Case1 "'Case1'" NoEncodeGeneric
  checkEncodingsWith options2 (Case2 1) "{'tag':'Case2','contents':1}" EncodeGeneric
  checkEncodingsWith options3 (Case3 1) "{'Case3':1}" EncodeGeneric
  checkEncodingsWith options4 (Case4 1) "1" EncodeGeneric
  checkEncodingsWith options5 Case5 "'_Case5'" NoEncodeGeneric
  checkDecodingWith options5 "'_Case5'" Case5 NoDecodeGeneric
  checkEncodingsWith options6 (Case6 1) "['Case6',1]" EncodeGeneric
  checkEncodingsWith options7 (Case7 1) "{'Case7':1}" EncodeGeneric
  checkEncodingsWith options8 (Case8 1) "['Case8', 1]" EncodeGeneric
  checkEncodingsWith options9 (Case9 1) "{'case9Int':1}" EncodeGeneric
  checkEncodingsWith options10 (Case10 1 "a") "{'tag':'_Case10', '_case10Int':1, '_case10Text':'a'}" EncodeGeneric
  checkDecodingWith options10 "{'tag':'_Case10', '_case10Int':1, '_case10Text':'a'}" (Case10 1 "a") DecodeGeneric
  checkEncodingsWith options11 (Case11 1 "a") "[1,'a']" EncodeGeneric
  checkDecodingWith options11 "[1,'a']" (Case11 1 "a") DecodeGeneric
  checkEncodingsWith options12 Case12_1 "{'a':'Case12_1'}" EncodeGeneric
  checkDecodingWith options12 "{'a':'Case12_1'}" Case12_1 DecodeGeneric
  checkEncodingsWith options13 (Case13_1 1 "a" True) "[1,'a', true]" EncodeGeneric
  checkEncodingsWith options13 (Case13_2 "a" True 1) "['a', true, 1]" EncodeGeneric
  checkEncodingsWith options14 (Case14_2 "a") "'a'" EncodeGeneric
  checkDecodingWith options14 "'a'" (Case14_2 "a") DecodeGeneric
  checkEncodingsWith options15 Case15_2 "'_Case15_2'" EncodeGeneric
  checkDecodingWith options15 "'_Case15_2'" Case15_2 DecodeGeneric
  checkEncodingsWith options15 Case15_2 "'_Case15_2'" EncodeGeneric

  checkDecodingWith defaultOptions {fieldLabelModifier = labelModifier, allNullaryToStringTag = False, sumEncoding = UntaggedValue} "{'_t12String':'a'}" (T12_2 "a") NoDecodeGeneric
  checkDecodingWith defaultOptions {fieldLabelModifier = labelModifier, allNullaryToStringTag = False, sumEncoding = UntaggedValue} "[{'_t12String':'a'},[1,{'_t2Int':1}]]" (T16_2 (T12_2 "a") (T15_1 (T1 1) (T2 1))) NoDecodeGeneric

  checkEncodingsWith options16 Case16 "{'tag':'Case16'}" NoEncodeGeneric
  checkDecodingWith options16 "{'tag':'Case16'}"  Case16 NoDecodeGeneric

-- * HELPERS

roundtrip :: forall a. (Show a, Eq a, Typeable a) => PropertyT IO ()
roundtrip = withFrozenCallStack $ do
  a <- for_all @a
  options <- for_all @Options
  let encoder = make @(Encoder a) (val options <: encoders)
  let decoder = make @(Decoder a) (val options <: decoders)
  let encoded = BL.fromStrict $ encodeByteString encoder a

  annotateShow encoded
  decodeByteString decoder encoded === Right a

-- Encoders

encoders =
  end
    <: $(makeEncoder ''T16)
    <: $(makeEncoder ''T15)
    <: $(makeEncoder ''T14)
    <: $(makeEncoder ''T13)
    <: $(makeEncoder ''T12)
    <: $(makeEncoder ''T11)
    <: $(makeEncoder ''T10)
    <: $(makeEncoder ''T9)
    <: $(makeEncoder ''T8)
    <: $(makeEncoder ''T7)
    <: $(makeEncoder ''T6)
    <: $(makeEncoder ''T5)
    <: $(makeEncoder ''T4)
    <: $(makeEncoder ''T3)
    <: $(makeEncoder ''T2)
    <: $(makeEncoder ''T1)
    <: $(makeEncoder ''T0)
    <: $(makeEncoder ''Case16)
    <: $(makeEncoder ''Case15)
    <: $(makeEncoder ''Case14)
    <: $(makeEncoder ''Case13)
    <: $(makeEncoder ''Case12)
    <: $(makeEncoder ''Case11)
    <: $(makeEncoder ''Case10)
    <: $(makeEncoder ''Case9)
    <: $(makeEncoder ''Case8)
    <: $(makeEncoder ''Case7)
    <: $(makeEncoder ''Case6)
    <: $(makeEncoder ''Case5)
    <: $(makeEncoder ''Case4)
    <: $(makeEncoder ''Case3)
    <: $(makeEncoder ''Case2)
    <: $(makeEncoder ''Case1)
    <: encodeMaybeOf @Int
    <: jsonEncoder @Text
    <: jsonEncoder @String
    <: jsonEncoder @Int
    <: jsonEncoder @Bool
    <: defaultEncoderOptions

-- Decoders

decoders =
  end
    <: $(makeDecoder ''T16)
    <: $(makeDecoder ''T15)
    <: $(makeDecoder ''T14)
    <: $(makeDecoder ''T13)
    <: $(makeDecoder ''T12)
    <: $(makeDecoder ''T11)
    <: $(makeDecoder ''T10)
    <: $(makeDecoder ''T9)
    <: $(makeDecoder ''T8)
    <: $(makeDecoder ''T7)
    <: $(makeDecoder ''T6)
    <: $(makeDecoder ''T5)
    <: $(makeDecoder ''T4)
    <: $(makeDecoder ''T3)
    <: $(makeDecoder ''T2)
    <: $(makeDecoder ''T1)
    <: $(makeDecoder ''T0)
    <: $(makeDecoder ''Case16)
    <: $(makeDecoder ''Case15)
    <: $(makeDecoder ''Case14)
    <: $(makeDecoder ''Case13)
    <: $(makeDecoder ''Case12)
    <: $(makeDecoder ''Case11)
    <: $(makeDecoder ''Case10)
    <: $(makeDecoder ''Case9)
    <: $(makeDecoder ''Case8)
    <: $(makeDecoder ''Case7)
    <: $(makeDecoder ''Case6)
    <: $(makeDecoder ''Case5)
    <: $(makeDecoder ''Case4)
    <: $(makeDecoder ''Case5)
    <: $(makeDecoder ''Case2)
    <: $(makeDecoder ''Case1)
    <: decodeMaybeOf @Int
    <: jsonDecoder @Text
    <: jsonDecoder @String
    <: jsonDecoder @Int
    <: jsonDecoder @Bool
    <: defaultDecoderOptions

-- Generators

for_all :: forall a. (Show a, Typeable a) => PropertyT IO a
for_all = forAll $ make @(Gen a) generators

generators =
  tweak @(Gen SumEncoding) (fmap adjustTaggedObject) $
    $(makeGenerators ''T16)
      <: $(makeGenerators ''T15)
      <: $(makeGenerators ''T14)
      <: $(makeGenerators ''T13)
      <: $(makeGenerators ''T12)
      <: $(makeGenerators ''T11)
      <: $(makeGenerators ''T10)
      <: $(makeGenerators ''T9)
      <: genFun T8
      <: genFun T7
      <: genFun T6
      <: genFun T5
      <: genFun T4
      <: genFun T3
      <: genFun T2
      <: genFun T1
      <: genVal (pure T0)
      <: $(makeGenerators ''Options)
      <: $(makeGenerators ''SumEncoding)
      <: genListOf @String
      <: genVal genText
      <: genVal genString
      <: genVal genInt
      <: genVal bool
      <: genVal (pure labelModifier)

-- | make sure the the tag and content fields are different
adjustTaggedObject :: SumEncoding -> SumEncoding
adjustTaggedObject (TaggedObject t c) = if t == c then TaggedObject t ("_" <> c) else TaggedObject t c
adjustTaggedObject other = other

genInt :: Gen Int
genInt = integral (linear 1 3)

genText :: Gen Text
genText = text (linear 1 10) alpha

genString :: Gen String
genString = toS <$> genText

checkEncodingsWith :: forall a. (ToJSON a, Typeable a) => Options -> a -> Text -> EncodeGeneric -> PropertyT IO ()
checkEncodingsWith options a expectShort encodeGeneric = withFrozenCallStack $ do
  let expected = T.replace "'" "\"" expectShort
  let encoder = make @(Encoder a) (val options <: encoders)
  let asValue = BL.toStrict . A.encode $ encodeValue encoder a
  let asEncoding = encodeByteString encoder a
  let asGeneric = BL.toStrict $ A.encode a

  when (encodeGeneric == EncodeGeneric) $ do
    annotate "the encoded Value must be the same as the generic one"
    checkValue asValue asGeneric

  annotate "the encoded Value must be the expected value"
  checkValue asValue (T.encodeUtf8 expected)

  annotate "the encoded Value must be the same as the one using a direct encoding"
  checkValue asValue asEncoding

checkDecodingWith :: forall a. (FromJSON a, ToJSON a, Typeable a, Eq a, Show a) => Options -> Text -> a -> DecodeGeneric -> PropertyT IO ()
checkDecodingWith options t a decodeGeneric = withFrozenCallStack $ do
  let input = BL.fromStrict . T.encodeUtf8 $ T.replace "'" "\"" t
  let decoder = make @(Decoder a) (val options <: decoders)
  let asValue = decodeByteString decoder input

  when (decodeGeneric == DecodeGeneric) $ do
    annotate "the decoded Value must be the same as the generic one"
    let asGeneric = A.decode input
    annotateShow (A.encode a)
    asValue === maybe (Left "the generic value cannot be decoded") Right asGeneric

  annotate "the decoded Value must be the expected value"
  asValue === Right a

data EncodeGeneric = EncodeGeneric | NoEncodeGeneric deriving (Eq, Show)
data DecodeGeneric = DecodeGeneric | NoDecodeGeneric deriving (Eq, Show)
