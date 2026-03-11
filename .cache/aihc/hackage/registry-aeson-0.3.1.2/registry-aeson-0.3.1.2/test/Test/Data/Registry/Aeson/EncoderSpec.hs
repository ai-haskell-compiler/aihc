{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.EncoderSpec where

import Data.Aeson hiding (encode)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL (fromStrict, toStrict)
import Data.Registry
import Data.Registry.Aeson.Encoder
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Protolude
import Test.Data.Registry.Aeson.DataTypes
import Test.Data.Registry.Aeson.SimilarDataTypes qualified as SimilarDataTypes
import Test.Tasty.Hedgehogx hiding (string)

test_encode = test "encode" $ do
  checkEncodings (Identifier 123) "123"
  checkEncodings email1 "{'_email':'me@here.com'}"
  checkEncodings delivery0 "{'tag':'NoDelivery'}"
  checkEncodings delivery1 "{'tag':'ByEmail','contents':{'_email':'me@here.com'}}"
  checkEncodings person1 "{'email':{'_email':'me@here.com'},'identifier':123}"
  checkEncodings delivery2 "{'tag':'InPerson','contents':[{'email':{'_email':'me@here.com'},'identifier':123},{'_datetime':'2022-04-18T00:00:12Z'}]}"

test_all_nullary_to_string_tag = test "allNullaryToStringTag" $ do
  checkEncodingsWith allNullaryOptions AllNullary1 "'AllNullary1'"
  checkEncodingsWith allNullaryOptions AllNullary2 "'AllNullary2'"

test_field_modifier = test "fieldLabelModifier" $ do
  checkEncodingsWith fieldLabelModifierOptions (FieldLabelModifier1 123) "{'tag':'FieldLabelModifier1','__field1':123}"

test_constructor_modifier = test "constructorTagModifier" $ do
  checkEncodingsWith constructorTagModifierOptions (ConstructorTagModifier1 123) "{'tag':'__ConstructorTagModifier1','ctField1':123}"

test_omit_nothing_fields = test "omitNothingFields" $ do
  checkEncodingsWith (defaultOptions {omitNothingFields = True}) (Identifier 123) "123"
  checkEncodingsWith omitNothingFieldsOptions (OmitNothingFields1 Nothing 123) "{'tag':'OmitNothingFields1','onField2':123}"

test_unwrap_unary_records = test "unwrapUnaryRecords" $ do
  checkEncodingsWith unwrapUnaryRecordsOptions (UnwrapUnaryRecords1 123) "123"

test_tag_single_constructors = test "TagSingleConstructors" $ do
  checkEncodingsWith tagSingleConstructorsOptions (TagSingleConstructors1 123) "{'tag':'TagSingleConstructors1', 'tsField1':123}"

test_untagged_values_sum_encoding = test "UntaggedValueSumEncoding" $ do
  checkEncodingsWith untaggedValueOptions (UntaggedValueSumEncoding1 123) "{'uvField1':123}"

test_object_with_single_field_sum_encoding = test "ObjectWithSingleFieldSumEncoding" $ do
  checkEncodingsWith objectWithSingleFieldSumEncodingOptions (ObjectWithSingleFieldSumEncoding1 123) "{'ObjectWithSingleFieldSumEncoding1':{'owsfField1':123}}"

test_two_elem_array_sum_encoding = test "TwoElemArray" $ do
  checkEncodingsWith twoElemArraySumEncodingOptions (TwoElemArraySumEncoding1 123) "['TwoElemArraySumEncoding1',{'teaField1':123}]"

test_types_th_index_error = test "error with TH when 2 fields have the same type" $ do
  -- this code did not compile before
  let _ = $(makeEncoder ''Stats) <: jsonEncoder @Int <: defaultEncoderOptions
  success

test_encode_map = test "encode maps" $ do
  let es =
        encodeMapOf @Name @Int
          <: jsonEncoder @Int
          <: jsonEncoder @Text
          <: encodeKey _name
          <: defaultEncoderOptions
  let bs = encodeByteString (make @(Encoder (Map Name Int)) es) [("name1", 1), ("name2", 2)]

  -- we check both the encoding and the application of the Options for
  checkValue bs "{\"name1\":1,\"name2\":2}"

-- * HELPERS

encoders :: Registry _ _
encoders =
  $(makeEncoder ''Delivery)
    -- test that it is possible to generate an Encoder when there are name clashes
    <: $(makeEncoderQualifiedLast ''SimilarDataTypes.Person)
    <: $(makeEncoderQualifiedLast ''SimilarDataTypes.Email)
    <: $(makeEncoderQualifiedLast ''SimilarDataTypes.Identifier)
    <: $(makeEncoderQualifiedLast ''SimilarDataTypes.DateTime)
    <: $(makeEncoder ''Person)
    <: $(makeEncoder ''Email)
    <: $(makeEncoder ''Identifier)
    <: $(makeEncoder ''AllNullary)
    <: $(makeEncoder ''FieldLabelModifier)
    <: $(makeEncoder ''ConstructorTagModifier)
    <: $(makeEncoder ''OmitNothingFields)
    <: $(makeEncoder ''UnwrapUnaryRecords)
    <: $(makeEncoder ''TagSingleConstructors)
    <: $(makeEncoder ''UntaggedValueSumEncoding)
    <: $(makeEncoder ''ObjectWithSingleFieldSumEncoding)
    <: $(makeEncoder ''TwoElemArraySumEncoding)
    <: fun datetimeEncoder
    <: fun utcTimeEncoder
    <: encodeMaybeOf @Int
    <: jsonEncoder @Text
    <: jsonEncoder @Int
    <: defaultEncoderOptions

-- | This Encoder for DateTime reproduces the default generic one
datetimeEncoder :: Encoder DateTime
datetimeEncoder = fromValue $ \(DateTime dt) -> do
  let formatted = toS $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dt
  Object [("_datetime", String formatted)]

-- | This Encoder for DateTime reproduces the default generic one
utcTimeEncoder :: Encoder UTCTime
utcTimeEncoder = fromValue $ String . toS . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Check that the encoding performed with the registry and the one performed with a Generic instance are the same
--   This helps validating the encoding algorithm based on the various Options values
--   Additionally we check that the bytestring created with an Encoding (with encodeByteString)
--   is the same as the one produced from a Value (with encodeValue)
checkEncodings :: forall a. (ToJSON a, Typeable a) => a -> Text -> PropertyT IO ()
checkEncodings a t = withFrozenCallStack $ checkEncodingsWith defaultOptions a t

checkEncodingsWith :: forall a. (ToJSON a, Typeable a) => Options -> a -> Text -> PropertyT IO ()
checkEncodingsWith options a expectShort = withFrozenCallStack $ do
  let expected = T.replace "'" "\"" expectShort
  let encoder = make @(Encoder a) (val options <: encoders)
  let asValue = BL.toStrict . A.encode $ encodeValue encoder a
  let asEncoding = encodeByteString encoder a
  let asGeneric = BL.toStrict $ A.encode a

  annotate "the encoded Value must be the same as the generic one"
  checkValue asValue asGeneric

  annotate "the encoded Value must be the expected value"
  checkValue asValue (T.encodeUtf8 expected)

  annotate "the encoded Value must be the same as the one using a direct encoding"
  checkValue asValue asEncoding

checkValue :: ByteString -> ByteString -> PropertyT IO ()
checkValue actual expected = withFrozenCallStack $ do
  case (decode @Value $ BL.fromStrict actual, decode $ BL.fromStrict expected) of
    (Just a, Just e) -> if a == e then success else actual === expected
    (actualValue, expectedValue) -> annotateShow ("cannot decode values", actual, expected, actualValue, expectedValue) >> failure
