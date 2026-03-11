{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.RoundtripData where

import Data.Aeson
import Protolude hiding (bool)
import Prelude (String)

data T0 = T0
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)

newtype T1 = T1 Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype T2 = T2 {t2Int :: Int}
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data T3 = T3 Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T4 = T4 {t4Int :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T5 = T5 Int Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T6 = T6 {t6Int :: Int, t6Text :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T7 = T7 Int Text Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T8 = T8 {t8Int :: Int, t8Text :: Text, t8Bool :: Bool}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T9 = T9_1 | T9_2 | T9_3
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T10 = T10_1 Int | T10_2 Text | T10_3 Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T11 = T11_1 {t11Int :: Int} | T11_2 {t11Text :: Text} | T11_3 {t11Bool :: Bool}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T12 = T12_1 Int | T12_2 {t12String :: String} | T12_3 Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T13 = T13_1 Int Text Bool | T13_2 Text Bool Int | T13_3 Int Bool Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T14 = T14_1 Int Text Bool | T14_2 Text Bool Int | T14_3
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T15 = T15_1 T1 T2 | T15_2 T13 T14 | T15_3 T11
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data T16 = T16_1 T9 T15 | T16_2 T12 T15
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- REGRESSION CASES
labelModifier :: String -> String
labelModifier s = "_" <> s

data Case1 = Case1
  deriving (Eq, Show, Generic)

options1 = defaultOptions {allNullaryToStringTag = True}

instance ToJSON Case1 where toJSON = genericToJSON options1

instance FromJSON Case1 where parseJSON = genericParseJSON options1

data Case2 = Case2 Int
  deriving (Eq, Show, Generic)

options2 = defaultOptions {tagSingleConstructors = True}

instance ToJSON Case2 where toJSON = genericToJSON options2

instance FromJSON Case2 where parseJSON = genericParseJSON options2

newtype Case3 = Case3 Int
  deriving (Eq, Show, Generic)

options3 = defaultOptions {sumEncoding = ObjectWithSingleField, tagSingleConstructors = True}

instance ToJSON Case3 where toJSON = genericToJSON options3

instance FromJSON Case3 where parseJSON = genericParseJSON options3

data Case4 = Case4 {case4Int :: Int}
  deriving (Eq, Show, Generic)

options4 = defaultOptions {sumEncoding = UntaggedValue, unwrapUnaryRecords = True, tagSingleConstructors = True}

instance ToJSON Case4 where toJSON = genericToJSON options4

instance FromJSON Case4 where parseJSON = genericParseJSON options4

data Case5 = Case5
  deriving (Eq, Show, Generic)

options5 = defaultOptions {constructorTagModifier = labelModifier, allNullaryToStringTag = True}

instance ToJSON Case5 where toJSON = genericToJSON options5

instance FromJSON Case5 where parseJSON = genericParseJSON options5

data Case6 = Case6 Int
  deriving (Eq, Show, Generic)

options6 = defaultOptions {sumEncoding = TwoElemArray, tagSingleConstructors = True}

instance ToJSON Case6 where toJSON = genericToJSON options6

instance FromJSON Case6 where parseJSON = genericParseJSON options6

data Case7 = Case7 {case7Int :: Int}
  deriving (Eq, Show, Generic)

options7 = defaultOptions {sumEncoding = ObjectWithSingleField, unwrapUnaryRecords = True, tagSingleConstructors = True}

instance ToJSON Case7 where toJSON = genericToJSON options7

instance FromJSON Case7 where parseJSON = genericParseJSON options7

data Case8 = Case8 {case8Int :: Int}
  deriving (Eq, Show, Generic)

options8 = defaultOptions {sumEncoding = TwoElemArray, unwrapUnaryRecords = True, tagSingleConstructors = True}

instance ToJSON Case8 where toJSON = genericToJSON options8

instance FromJSON Case8 where parseJSON = genericParseJSON options8

data Case9 = Case9 {case9Int :: Int}
  deriving (Eq, Show, Generic)

options9 = defaultOptions {allNullaryToStringTag = False, omitNothingFields = False, sumEncoding = UntaggedValue, unwrapUnaryRecords = False, tagSingleConstructors = True, rejectUnknownFields = False}

instance ToJSON Case9 where toJSON = genericToJSON options9

instance FromJSON Case9 where parseJSON = genericParseJSON options9

data Case10 = Case10 {case10Int :: Int, case10Text :: Text}
  deriving (Eq, Show, Generic)

options10 = defaultOptions {fieldLabelModifier = labelModifier, constructorTagModifier = labelModifier, allNullaryToStringTag = False, omitNothingFields = False, sumEncoding = TaggedObject {tagFieldName = "tag", contentsFieldName = "a"}, unwrapUnaryRecords = False, tagSingleConstructors = True, rejectUnknownFields = False}

instance ToJSON Case10 where toJSON = genericToJSON options10

instance FromJSON Case10 where parseJSON = genericParseJSON options10

data Case11 = Case11 Int Text
  deriving (Eq, Show, Generic)

options11 = defaultOptions {sumEncoding = UntaggedValue, tagSingleConstructors = True}

instance ToJSON Case11 where toJSON = genericToJSON options11

instance FromJSON Case11 where parseJSON = genericParseJSON options11

data Case12 = Case12_1 | Case12_2
  deriving (Eq, Show, Generic)

options12 = defaultOptions {allNullaryToStringTag = False, omitNothingFields = False, sumEncoding = TaggedObject {tagFieldName = "a", contentsFieldName = "_a"}, unwrapUnaryRecords = False, tagSingleConstructors = False, rejectUnknownFields = False}

instance ToJSON Case12 where toJSON = genericToJSON options12

instance FromJSON Case12 where parseJSON = genericParseJSON options12

data Case13 = Case13_1 Int Text Bool | Case13_2 Text Bool Int
  deriving (Eq, Show, Generic)

options13 = defaultOptions {sumEncoding = UntaggedValue}

instance ToJSON Case13 where toJSON = genericToJSON options13

instance FromJSON Case13 where parseJSON = genericParseJSON options13

data Case14 = Case14_1 Int | Case14_2 {case14Text :: Text} | Case14_3 Bool
  deriving (Eq, Show, Generic)

options14 = defaultOptions {sumEncoding = UntaggedValue, unwrapUnaryRecords = True}

instance ToJSON Case14 where toJSON = genericToJSON options14

instance FromJSON Case14 where parseJSON = genericParseJSON options14

data Case15 = Case15_1 | Case15_2 | Case15_3
  deriving (Eq, Show, Generic)

options15 = defaultOptions {constructorTagModifier = labelModifier, sumEncoding = UntaggedValue}

instance ToJSON Case15 where toJSON = genericToJSON options15

instance FromJSON Case15 where parseJSON = genericParseJSON options15

data Case16 = Case16
  deriving (Eq, Show, Generic)

options16 = defaultOptions {allNullaryToStringTag = False}

instance ToJSON Case16 where toJSON = genericToJSON options16

instance FromJSON Case16 where parseJSON = genericParseJSON options16
