{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.CoerceSpec
    ( spec
    ) where

import Data.Aeson as Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isNothing)
import Data.Scientific (scientific)
import qualified Language.GraphQL.Execute.Coerce as Coerce
import Language.GraphQL.JSON (JSON(..))
import qualified Language.GraphQL.Type.In as In
import Language.GraphQL.Type
import Prelude hiding (id)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

singletonInputObject :: In.Type
singletonInputObject = In.NamedInputObjectType type'
  where
    type' = In.InputObjectType "ObjectName" Nothing inputFields
    inputFields = HashMap.singleton "field" field
    field = In.InputField Nothing (In.NamedScalarType string) Nothing

namedIdType :: In.Type
namedIdType = In.NamedScalarType id

spec :: Spec
spec =
    describe "VariableValue Aeson" $ do
        it "coerces strings" $
            let expected = Just (String "asdf")
                actual = Coerce.coerceVariableValue (In.NamedScalarType string)
                    $ JSON $ Aeson.String "asdf"
             in actual `shouldBe` expected
        it "coerces non-null strings" $
            let expected = Just (String "asdf")
                actual = Coerce.coerceVariableValue (In.NonNullScalarType string)
                    $ JSON $ Aeson.String "asdf"
             in actual `shouldBe` expected
        it "coerces booleans" $
            let expected = Just (Boolean True)
                actual = Coerce.coerceVariableValue (In.NamedScalarType boolean)
                    $ JSON $ Aeson.Bool True
             in actual `shouldBe` expected
        it "coerces zero to an integer" $
            let expected = Just (Int 0)
                actual = Coerce.coerceVariableValue (In.NamedScalarType int)
                    $ JSON $ Aeson.Number 0
             in actual `shouldBe` expected
        it "rejects fractional if an integer is expected" $
            let actual = Coerce.coerceVariableValue (In.NamedScalarType int)
                    $ JSON $ Aeson.Number $ scientific 14 (-1)
             in actual `shouldSatisfy` isNothing
        it "coerces float numbers" $
            let expected = Just (Float 1.4)
                actual = Coerce.coerceVariableValue (In.NamedScalarType float)
                    $ JSON $ Aeson.Number $ scientific 14 (-1)
             in actual `shouldBe` expected
        it "coerces IDs" $
            let expected = Just (String "1234")
                json = JSON $ Aeson.String "1234"
                actual = Coerce.coerceVariableValue namedIdType json
             in actual `shouldBe` expected
        it "coerces input objects" $
            let actual = Coerce.coerceVariableValue singletonInputObject
                    $ JSON
                    $ Aeson.object ["field" .= ("asdf" :: Aeson.Value)]
                expected = Just $ Object $ HashMap.singleton "field" "asdf"
             in actual `shouldBe` expected
        it "skips the field if it is missing in the variables" $
            let actual = Coerce.coerceVariableValue singletonInputObject
                    $ JSON Aeson.emptyObject
                expected = Just $ Object HashMap.empty
             in actual `shouldBe` expected
        it "fails if input object value contains extra fields" $
            let actual = Coerce.coerceVariableValue singletonInputObject
                    $ JSON $ Aeson.object variableFields
                variableFields =
                    [ "field" .= ("asdf" :: Aeson.Value)
                    , "extra" .= ("qwer" :: Aeson.Value)
                    ]
             in actual `shouldSatisfy` isNothing
        it "preserves null" $
            let actual = Coerce.coerceVariableValue namedIdType
                    $ JSON Aeson.Null
             in actual `shouldBe` Just Null
        it "preserves list order" $
            let list = JSON $ Aeson.toJSONList ["asdf" :: Aeson.Value, "qwer"]
                listType = (In.ListType $ In.NamedScalarType string)
                actual = Coerce.coerceVariableValue listType list
                expected = Just $ List [String "asdf", String "qwer"]
             in actual `shouldBe` expected
