{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.EDN.QQ.Test
  ( tests
  ) where

import Hedgehog (Group, Property, discover, property, withTests, (===))

import qualified Data.EDN as EDN
import qualified Data.EDN.QQ as QQ

prop_tagged_value :: Property
prop_tagged_value = withTests 1 . property $
  EDN.Tagged "test" "tagged" EDN.Nil === [QQ.edn| #test/tagged nil |]

prop_list :: Property
prop_list = withTests 1 . property $ do
  EDN.mkList items === [QQ.ednList|
    false,
    42
    #my/tagged symbol
  |]

prop_vector :: Property
prop_vector = withTests 1 . property $ do
  EDN.mkVec items === [QQ.ednVec|
    false,
    42
    #my/tagged symbol
  |]

prop_set :: Property
prop_set = withTests 1 . property $ do
  EDN.mkSet items === [QQ.ednSet|
    false,
    42
    #my/tagged symbol
  |]

prop_map :: Property
prop_map = withTests 1 . property $ do
  EDN.mkMap pairs === [QQ.ednMap|
    this/is false,
    :the-answer 42
    #my/tagged key value
  |]
  where
    pairs =
      [ ( EDN.NoTag $ EDN.Symbol "this" "is"
        , EDN.NoTag $ EDN.Boolean False)
      , ( EDN.NoTag $ EDN.Keyword "the-answer"
        , EDN.NoTag $ EDN.Integer 42
        )
      , ( EDN.Tagged "my" "tagged" $ EDN.Symbol "" "key"
        , EDN.NoTag $ EDN.Symbol "" "value"
        )
      ]

items :: [EDN.TaggedValue]
items =
  [ EDN.NoTag $ EDN.Boolean False
  , EDN.NoTag $ EDN.Integer 42
  , EDN.Tagged "my" "tagged" $ EDN.Symbol "" "symbol"
  ]

-- XXX: type application must be in some other module.
-- prop_from_value :: Property
-- prop_from_value = withTests 1 . property $ do
--   () === [fromEDN @()| nil |]

tests :: Group
tests = $$(discover)
