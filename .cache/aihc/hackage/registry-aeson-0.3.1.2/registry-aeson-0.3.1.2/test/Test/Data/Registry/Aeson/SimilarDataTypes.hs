{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This modules helps testing that we can use TemplateHaskell when there are name clashes
module Test.Data.Registry.Aeson.SimilarDataTypes where

import Data.Aeson
import Data.Time
import Protolude

newtype Identifier = Identifier Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Email = Email {_email :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DateTime = DateTime {_datetime :: UTCTime}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Person = Person {identifier :: Identifier, email :: Email}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Team = Team {name :: Text, members :: [Person], leaderName :: Maybe Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Delivery
  = NoDelivery
  | ByEmail Email
  | InPerson Person DateTime
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Path
  = File Int
  | Directory [Path]
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- * EXAMPLES

email1 :: Email
email1 = Email "me@here.com"

person1 :: Person
person1 = Person (Identifier 123) email1

delivery0 :: Delivery
delivery0 = NoDelivery

delivery1 :: Delivery
delivery1 = ByEmail email1

delivery2 :: Delivery
delivery2 = InPerson person1 datetime1

datetime1 :: DateTime
datetime1 = DateTime $ UTCTime (fromGregorian 2022 4 18) 12

path1 :: Path
path1 = Directory [Directory [Directory [File 1], Directory [File 2]], File 3]

-- * OPTIONS TEST

allNullaryOptions = defaultOptions {allNullaryToStringTag = True}

data AllNullary
  = AllNullary1
  | AllNullary2
  deriving (Eq, Show, Generic)

instance ToJSON AllNullary where
  toJSON = genericToJSON allNullaryOptions

instance FromJSON AllNullary where
  parseJSON = genericParseJSON allNullaryOptions

fieldLabelModifierOptions = defaultOptions {fieldLabelModifier = ("__" <>)}

data FieldLabelModifier
  = FieldLabelModifier1 {field1 :: Int}
  | FieldLabelModifier2 {field2 :: Int}
  deriving (Eq, Show, Generic)

instance ToJSON FieldLabelModifier where
  toJSON = genericToJSON fieldLabelModifierOptions

instance FromJSON FieldLabelModifier where
  parseJSON = genericParseJSON fieldLabelModifierOptions

constructorTagModifierOptions = defaultOptions {constructorTagModifier = ("__" <>)}

data ConstructorTagModifier
  = ConstructorTagModifier1 {ctField1 :: Int}
  | ConstructorTagModifier2 {ctField2 :: Int}
  deriving (Eq, Show, Generic)

instance ToJSON ConstructorTagModifier where
  toJSON = genericToJSON constructorTagModifierOptions

instance FromJSON ConstructorTagModifier where
  parseJSON = genericParseJSON constructorTagModifierOptions

omitNothingFieldsOptions = defaultOptions {omitNothingFields = True}

data OmitNothingFields
  = OmitNothingFields1 {onField1 :: Maybe Int, onField2 :: Int}
  | OmitNothingFields2 {onField3 :: Int}
  deriving (Eq, Show, Generic)

instance ToJSON OmitNothingFields where
  toJSON = genericToJSON omitNothingFieldsOptions

instance FromJSON OmitNothingFields where
  parseJSON = genericParseJSON omitNothingFieldsOptions

unwrapUnaryRecordsOptions = defaultOptions {unwrapUnaryRecords = True}

newtype UnwrapUnaryRecords = UnwrapUnaryRecords1 {uuField1 :: Int}
  deriving (Eq, Show, Generic)

instance ToJSON UnwrapUnaryRecords where
  toJSON = genericToJSON unwrapUnaryRecordsOptions

instance FromJSON UnwrapUnaryRecords where
  parseJSON = genericParseJSON unwrapUnaryRecordsOptions

tagSingleConstructorsOptions = defaultOptions {tagSingleConstructors = True}

newtype TagSingleConstructors = TagSingleConstructors1 {tsField1 :: Int}
  deriving (Eq, Show, Generic)

instance ToJSON TagSingleConstructors where
  toJSON = genericToJSON tagSingleConstructorsOptions

instance FromJSON TagSingleConstructors where
  parseJSON = genericParseJSON tagSingleConstructorsOptions

untaggedValueOptions = defaultOptions {sumEncoding = UntaggedValue}

data UntaggedValueSumEncoding
  = UntaggedValueSumEncoding1 {uvField1 :: Int}
  | UntaggedValueSumEncoding2 {uvField2 :: Int}
  deriving (Eq, Show, Generic)

instance ToJSON UntaggedValueSumEncoding where
  toJSON = genericToJSON untaggedValueOptions

instance FromJSON UntaggedValueSumEncoding where
  parseJSON = genericParseJSON untaggedValueOptions

objectWithSingleFieldSumEncodingOptions = defaultOptions {sumEncoding = ObjectWithSingleField}

data ObjectWithSingleFieldSumEncoding
  = ObjectWithSingleFieldSumEncoding1 {owsfField1 :: Int}
  | ObjectWithSingleFieldSumEncoding2 {owsfField2 :: Int}
  deriving (Eq, Show, Generic)

instance ToJSON ObjectWithSingleFieldSumEncoding where
  toJSON = genericToJSON objectWithSingleFieldSumEncodingOptions

instance FromJSON ObjectWithSingleFieldSumEncoding where
  parseJSON = genericParseJSON objectWithSingleFieldSumEncodingOptions

twoElemArraySumEncodingOptions = defaultOptions {sumEncoding = TwoElemArray}

data TwoElemArraySumEncoding
  = TwoElemArraySumEncoding1 {teaField1 :: Int}
  | TwoElemArraySumEncoding2 {teaField2 :: Int}
  deriving (Eq, Show, Generic)

instance ToJSON TwoElemArraySumEncoding where
  toJSON = genericToJSON twoElemArraySumEncodingOptions

instance FromJSON TwoElemArraySumEncoding where
  parseJSON = genericParseJSON twoElemArraySumEncodingOptions
