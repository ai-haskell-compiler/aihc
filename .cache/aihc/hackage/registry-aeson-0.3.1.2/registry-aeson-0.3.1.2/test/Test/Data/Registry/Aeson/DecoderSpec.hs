{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.DecoderSpec where

import Data.Aeson hiding (decode)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL (ByteString, fromStrict)
import Data.Registry
import Data.Registry.Aeson.Decoder
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Protolude
import Test.Data.Registry.Aeson.DataTypes
import Test.Data.Registry.Aeson.SimilarDataTypes qualified as SimilarDataTypes
import Test.Tasty.Hedgehogx hiding (either, maybe, text)

test_decode = test "decode" $ do
  checkDecoding "123" (Identifier 123)
  checkDecoding "{'_email':'me@here.com'}" email1
  checkDecoding "{'email':{'_email':'me@here.com'},'identifier':123}" person1
  checkDecoding "{'_datetime':'2022-04-18T00:00:12Z'}" datetime1
  checkDecoding "{'tag':'NoDelivery'}" delivery0
  checkDecoding "{'tag':'ByEmail','contents':{'_email':'me@here.com'}}" delivery1
  checkDecoding "{'tag':'InPerson','contents':[{'email':{'_email':'me@here.com'},'identifier':123},{'_datetime':'2022-04-18T00:00:12Z'}]}" delivery2

test_all_nullary_to_string_tag = test "allNullaryToStringTag" $ do
  checkDecodingWith allNullaryOptions "'AllNullary1'" AllNullary1
  checkDecodingWith allNullaryOptions "'AllNullary2'" AllNullary2

test_field_modifier = test "fieldLabelModifier" $ do
  checkDecodingWith fieldLabelModifierOptions "{'tag':'FieldLabelModifier1','__field1':123}" (FieldLabelModifier1 123)

test_constructor_modifier = test "constructorTagModifier" $ do
  checkDecodingWith constructorTagModifierOptions "{'tag':'__ConstructorTagModifier1','ctField1':123}" (ConstructorTagModifier1 123)

test_omit_nothing_fields = test "omitNothingFields" $ do
  checkDecodingWith omitNothingFieldsOptions "{'tag':'OmitNothingFields1','onField2':123}" (OmitNothingFields1 Nothing 123)

test_unwrap_unary_records = test "unwrapUnaryRecords" $ do
  checkDecodingWith unwrapUnaryRecordsOptions "123" (UnwrapUnaryRecords1 123)

test_tag_single_constructors = test "TagSingleConstructors" $ do
  checkDecodingWith tagSingleConstructorsOptions "{'tag':'TagSingleConstructors1', 'tsField1':123}" (TagSingleConstructors1 123)

test_untagged_values_sum_encoding = test "UntaggedValueSumEncoding" $ do
  checkDecodingWith untaggedValueOptions "{'uvField1':123}" (UntaggedValueSumEncoding1 123)

test_object_with_single_field_sum_encoding = test "ObjectWithSingleFieldSumEncoding" $ do
  checkDecodingWith objectWithSingleFieldSumEncodingOptions "{'ObjectWithSingleFieldSumEncoding1':{'owsfField1':123}}" (ObjectWithSingleFieldSumEncoding1 123)

test_two_elem_array_sum_encoding = test "TwoElemArray" $ do
  checkDecodingWith twoElemArraySumEncodingOptions "['TwoElemArraySumEncoding1',{'teaField1':123}]" (TwoElemArraySumEncoding1 123)

test_errors = test "report errors" $ do
  checkErrors @Identifier "'123'" "Cannot decode the type 'Identifier' >> parsing Int failed, expected Number, but encountered String"
  checkErrors @Email "{'_email':123}" "Cannot decode the type 'Email' >> '_email :: Text' >> parsing Text failed, expected String, but encountered Number"
  checkErrors @Email "{'email':'me@here.com'}" "Cannot decode the type 'Email' >> field '_email' not found"
  checkErrors @Person "{'email':{'email':'me@here.com'},'identifier':123}" "Cannot decode the type 'Person' >> 'email :: Email' >> field '_email' not found"
  checkErrors @Person "{'email':{'email':'me@here.com'}}" "Cannot decode the type 'Person' >> field 'identifier' not found"
  checkErrors @Person "{'email':{'_email':123},'identifier':123}" "Cannot decode the type 'Person' >> 'email :: Email' >> '_email :: Text' >> parsing Text failed, expected String, but encountered Number"
  checkErrors @Team
    "{'name':'team1', 'members': [{'email':{'_email':'1'},'identifier':1}, {'email':{'_email':2},'identifier':2}], 'leaderName':'me'}"
    "Cannot decode the type 'Team' >> 'members :: List Person' >> 'email :: Email' >> '_email :: Text' >> parsing Text failed, expected String, but encountered Number"
  checkErrors @Team
    "{'name':'team1', 'members': [{'email':{'_email':'1'},'identifier':1}, {'email':{'_email':'2'},'identifier':2}], 'leaderName':123}"
    "Cannot decode the type 'Team' >> 'leaderName :: Maybe Text' >> parsing Text failed, expected String, but encountered Number"

  checkErrors @Delivery "{'tag':'NoDeliveryx'}" "Cannot decode the type 'Delivery' >> expected the tag field to be one of: NoDelivery, ByEmail, InPerson, found: NoDeliveryx"
  checkErrors @Delivery "{'tag1':'NoDelivery'}" "Cannot decode the type 'Delivery' >> tag field 'tag' not found"
  checkErrorsWith @Delivery
    constructorTagModifierOptions
    "{'tag':'_NoDelivery'}"
    "Cannot decode the type 'Delivery' >> expected the tag field to be one of: __NoDelivery, __ByEmail, __InPerson, found: _NoDelivery"

  checkErrors @Delivery
    "{'tag':'ByEmail','contents':{'_email':123}}"
    "Cannot decode the type 'Delivery' >> (ByEmail) '_email :: Text' >> parsing Text failed, expected String, but encountered Number"

  checkErrors @Delivery
    "{'tag':'InPerson','contents':[{'email':{'_email':'me@here.com'},'identifier':123},{'datetime':'2022-04-18T00:00:12Z'}]}"
    "Cannot decode the type 'Delivery' >> (InPerson) field '_datetime' not found"

  checkErrors @Delivery
    "{'tag':'InPerson','contents':[{'email':{'_email':'me@here.com'},'identifier':123},'2022-04-18T00:00:12Z']}"
    "Cannot decode the type 'Delivery' >> (InPerson) expected an object with field '_datetime"

test_reject_unknown_fields = test "rejectUnknownFields" $ do
  let reject = defaultOptions {rejectUnknownFields = True}
  checkErrorsWith @Email reject "{'_email':'me@here.com', 'f':1}" "Cannot decode the type 'Email' >> unknown field: f"
  checkErrorsWith @Delivery
    reject
    "{'tag':'ByEmail','contents':{'_email':'me@here.com','f':1}}"
    "Cannot decode the type 'Delivery' >> (ByEmail) unknown field: f"
  checkErrorsWith @Delivery
    reject {sumEncoding = UntaggedValue}
    "{'_email':'me@here.com','f':1}"
    "Cannot decode the type 'Delivery' >> (ByEmail) unknown field: f"
  checkErrorsWith @Delivery
    reject {sumEncoding = TwoElemArray}
    "['ByEmail', {'_email':'me@here.com','f':1}]"
    "Cannot decode the type 'Delivery' >> (ByEmail) unknown field: f"
  checkErrorsWith @Delivery
    reject {sumEncoding = ObjectWithSingleField}
    "{'ByEmail':{'_email':'me@here.com','f':1}}"
    "Cannot decode the type 'Delivery' >> (ByEmail) unknown field: f"
  checkErrorsWith @Delivery
    reject
    "{'tag':'InPerson','contents':[{'email':{'_email':'me@here.com'},'identifier':123,'f1':1,'f2':1},{'_datetime':'2022-04-18T00:00:12Z'}]}"
    "Cannot decode the type 'Delivery' >> (InPerson) unknown fields: f1, f2"
  checkErrorsWith @Delivery
    reject {sumEncoding = UntaggedValue}
    "[{'email':{'_email':'me@here.com'},'identifier':123,'f1':1,'f2':1},{'_datetime':'2022-04-18T00:00:12Z'}]"
    "Cannot decode the type 'Delivery' >> (ByEmail) expected an object with field '_email ->> (InPerson) unknown fields: f1, f2"
  checkErrorsWith @Delivery
    reject {sumEncoding = TwoElemArray}
    "['InPerson', [{'email':{'_email':'me@here.com'},'identifier':123,'f1':1,'f2':1},{'_datetime':'2022-04-18T00:00:12Z'}]]"
    "Cannot decode the type 'Delivery' >> (InPerson) unknown fields: f1, f2"
  checkErrorsWith @Delivery
    reject {sumEncoding = ObjectWithSingleField}
    "{'InPerson':[{'email':{'_email':'me@here.com'},'identifier':123,'f1':1,'f2':1},{'_datetime':'2022-04-18T00:00:12Z'}]}"
    "Cannot decode the type 'Delivery' >> (InPerson) unknown fields: f1, f2"

test_types_th_index_error = test "error with TH when 2 fields have the same type" $ do
  -- this code did not compile before
  let _ = $(makeDecoder ''Stats) <: jsonDecoder @Int <: defaultDecoderOptions
  success

test_decode_map = test "decode map" $ do
  let ds =
        decodeMapOf @Name @Int
          <: decodeKey (Right . Name)
          <: jsonDecoder @Int
          <: jsonDecoder @Text
          <: defaultDecoderOptions

  case decodeByteString (make @(Decoder (Map Name Int)) ds) "{\"name1\":1,\"name2\":2}" of
    Left e -> annotateShow e >> failure
    Right a -> a === [("name1", 1), ("name2", 2)]

-- * HELPERS

checkDecoding :: forall a. (FromJSON a, ToJSON a, Typeable a, Eq a, Show a) => Text -> a -> PropertyT IO ()
checkDecoding t a = withFrozenCallStack $ checkDecodingWith defaultOptions t a

checkDecodingWith :: forall a. (FromJSON a, ToJSON a, Typeable a, Eq a, Show a) => Options -> Text -> a -> PropertyT IO ()
checkDecodingWith options text a = withFrozenCallStack $ do
  let input = setDoubleQuotes text
  let decoder = make @(Decoder a) (val options <: decoders)
  let asValue = decodeByteString decoder input
  let asGeneric = A.decode input

  annotate "the decoded Value must be the same as the generic one"
  annotateShow (encode a)
  asValue === maybe (Left "wrong") Right asGeneric

  annotate "the decoded Value must be the expected value"
  asValue === Right a

checkErrors :: forall a. (FromJSON a, ToJSON a, Typeable a, Eq a, Show a) => Text -> Text -> PropertyT IO ()
checkErrors t1 t2 = withFrozenCallStack $ checkErrorsWith @a defaultOptions t1 t2

checkErrorsWith :: forall a. (FromJSON a, ToJSON a, Typeable a, Eq a, Show a) => Options -> Text -> Text -> PropertyT IO ()
checkErrorsWith options text errorMessage = withFrozenCallStack $ do
  let input = setDoubleQuotes text
  let decoder = make @(Decoder a) (val options <: decoders)
  let asValue = decodeByteString decoder input
  -- let asGeneric = A.eitherDecode @a input
  -- annotateShow asGeneric
  mapLeft setSimpleQuotes asValue === Left (setSimpleQuotes errorMessage)

decoders :: Registry _ _
decoders =
  $(makeDecoder ''Delivery)
    <: $(makeDecoder ''Team)
    <: decodeListOf @Person
    -- test that it is possible to generate a Decoder when there are name clashes
    <: $(makeDecoderQualifiedLast ''SimilarDataTypes.Person)
    <: $(makeDecoderQualifiedLast ''SimilarDataTypes.Email)
    <: $(makeDecoderQualifiedLast ''SimilarDataTypes.Identifier)
    <: $(makeDecoderQualifiedLast ''SimilarDataTypes.DateTime)
    <: $(makeDecoder ''Person)
    <: $(makeDecoder ''Email)
    <: $(makeDecoder ''Identifier)
    <: $(makeDecoder ''DateTime)
    <: $(makeDecoder ''AllNullary)
    <: $(makeDecoder ''FieldLabelModifier)
    <: $(makeDecoder ''ConstructorTagModifier)
    <: $(makeDecoder ''OmitNothingFields)
    <: $(makeDecoder ''UnwrapUnaryRecords)
    <: $(makeDecoder ''TagSingleConstructors)
    <: $(makeDecoder ''UntaggedValueSumEncoding)
    <: $(makeDecoder ''ObjectWithSingleFieldSumEncoding)
    <: $(makeDecoder ''TwoElemArraySumEncoding)
    <: fun utcTimeDecoder
    <: decodeMaybeOf @Text
    <: jsonDecoder @Text
    <: decodeMaybeOf @Int
    <: jsonDecoder @Int
    <: defaultDecoderOptions

utcTimeDecoder :: Decoder UTCTime
utcTimeDecoder = Decoder $ \case
  String s ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ toS s of
      Just t -> pure t
      Nothing -> Left ("cannot read a UTCTime: " <> s)
  other -> Left $ "not a valid UTCTime: " <> show other

setSimpleQuotes :: Text -> Text
setSimpleQuotes = T.replace "\"" "'"

setDoubleQuotesText :: Text -> Text
setDoubleQuotesText = T.replace "'" "\""

setDoubleQuotes :: Text -> BL.ByteString
setDoubleQuotes = BL.fromStrict . T.encodeUtf8 . setDoubleQuotesText

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left e) = Left (f e)
mapLeft _ (Right a) = Right a
