{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.EDN.Class.Test
  ( tests
  ) where

import Hedgehog
  ( Group, Property
  , discover, property, withTests
  , tripping
  , (===)
  )
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Data.UUID.Types (UUID)
import Data.Void (Void)

import qualified Data.Vector as Vector
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.EDN.Class (ToEDN(..), FromEDN(..), toEDNtagged, fromEDN)

import qualified Data.EDN as EDN

tests :: Group
tests = $$(discover)

type Example = Property

codecExample :: forall a. (ToEDN a, FromEDN a, Eq a, Show a) => Text -> a -> Example
codecExample encoded value = withTests 1 . property $ do
  tripping value toEDN fromEDN
  EDN.renderText (toEDN value) === encoded

-- * Atomic types

-- ** The Singleton Value

prop_nil_unit :: Example
prop_nil_unit = codecExample "nil" ()

-- ** Bools

prop_bool_true :: Example
prop_bool_true = codecExample "true" True

prop_bool_false :: Example
prop_bool_false = codecExample "false" False

-- ** Strings

prop_text :: Example
prop_text = codecExample
  "\"stringly \\\"typed\\\" languages\""
  ("stringly \"typed\" languages" :: Text)

-- ** Characters

prop_char_unicode_hex :: Example
prop_char_unicode_hex = codecExample "\\uA650" 'Ꙑ'

prop_char_named_WS :: Example
prop_char_named_WS = codecExample "\\space" ' '

prop_char_basic :: Example
prop_char_basic = codecExample "\\n" 'n'

-- ** Numeric

prop_int :: Example
prop_int = codecExample "42" (42 :: Int)

prop_double :: Example
prop_double = codecExample "42.0" (42.0 :: Double)

-- * Containers

items :: [EDN.Value]
items =
  [ toEDNv (1 :: Int)
  , toEDNv True
  , EDN.Symbol "sort-of" "hetero"
  ]

prop_nil_maybe :: Example
prop_nil_maybe = codecExample "nil" (Nothing :: Maybe Void)

prop_list_empty :: Example
prop_list_empty = codecExample "()" ([] :: [Void])

prop_list :: Example
prop_list = codecExample "(1 true sort-of/hetero)" items

prop_vector_empty :: Example
prop_vector_empty = codecExample "[]" (mempty :: Vector.Vector Void)

prop_vector :: Example
prop_vector = codecExample "[1 true sort-of/hetero]" $ Vector.fromList items

prop_set_empty :: Example
prop_set_empty = codecExample "#{}" (mempty :: Set.Set Void)

prop_set :: Example
prop_set = codecExample "#{true sort-of/hetero 1}" $ Set.fromList items

prop_map_empty :: Example
prop_map_empty = codecExample "{}" (mempty :: Map.Map () ())

prop_map :: Example
prop_map = codecExample "{the/question \\? :the-answer 42.0}" $ Map.fromList
  [ ( EDN.Keyword "the-answer"
    , EDN.Floating 42.0
    )
  , ( EDN.Symbol "the" "question"
    , EDN.Character '?'
    )
  ]

-- ** Tuples

prop_tuple_2 :: Example
prop_tuple_2 = codecExample "[true nil]" (True, ())

prop_tuple_3 :: Example
prop_tuple_3 = codecExample "[true nil \\space]" (True, (), ' ')

prop_tuple_4 :: Example
prop_tuple_4 = codecExample "[true nil \\space ()]" (True, (), ' ', [] :: [Void])

-- * Tagged

prop_tagged_utctime :: Example
prop_tagged_utctime = codecExample
#if MIN_VERSION_time(1,9,3)
  "#inst \"1985-04-12T23:20:50.52UTC\""
#else
  "#inst \"1985-04-12T23:20:50.52Z\""
#endif
  (parseTimeOrError
    False
    defaultTimeLocale
    "%Y-%m-%dT%H:%M:%S%Q%Z"
    "1985-04-12T23:20:50.52Z"
    :: UTCTime
  )

prop_tagged_uuid :: Example
prop_tagged_uuid = codecExample
  "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
  (read "f81d4fae-7dec-11d0-a765-00a0c91e6bf6" :: UUID)

-- ** User type

data Person = Person
  { personFirst :: Text
  , personLast  :: Text
  } deriving (Eq, Show)

instance ToEDN Person where
  toEDN Person{..} =
    toEDNtagged "myapp" "Person" $ Map.fromList
      [ (EDN.Keyword "first", toEDN personFirst)
      , (EDN.Keyword "last", toEDN personLast)
      ]

instance FromEDN Person where
  parseEDN =
    EDN.withTagged "myapp" "Person" . EDN.withMap $ \m -> Person
      <$> EDN.mapGetKeyword "first" m
      <*> EDN.mapGetKeyword "last" m

prop_tagged_person :: Example
prop_tagged_person = codecExample
  "#myapp/Person {:first \"Fred\" :last \"Mertz\"}"
  (Person "Fred" "Mertz")

-- * Expected stuff

prop_unexpected :: Property
prop_unexpected = property $ do
  uuid <- either fail pure $
    EDN.decodeText "Test.hs:184" "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
  uuid === (read "f81d4fae-7dec-11d0-a765-00a0c91e6bf6" :: UUID)
