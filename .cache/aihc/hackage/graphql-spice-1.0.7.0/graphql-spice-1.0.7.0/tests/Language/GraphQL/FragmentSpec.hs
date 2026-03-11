{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.FragmentSpec
    ( spec
    ) where

import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Language.GraphQL.AST (Name)
import Data.HashMap.Strict (HashMap)
import Language.GraphQL.Type
import Language.GraphQL.Error
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.TH
import qualified Language.GraphQL as GraphQL
import Test.Hspec (Spec, describe, it)
import Test.Hspec.GraphQL

size :: (Text, Value)
size = ("size", String "L")

circumference :: (Text, Value)
circumference = ("circumference", Int 60)

garment :: Text -> (Text, Value)
garment typeName =
    ("garment",  Object $ HashMap.fromList
        [ if typeName == "Hat" then circumference else size
        , ("__typename", String typeName)
        ]
    )

inlineQuery :: Text
inlineQuery = [gql|
  {
    garment {
      ... on Hat {
        circumference
      }
      ... on Shirt {
        size
      }
    }
  }
|]

shirtType :: Out.ObjectType IO
shirtType = Out.ObjectType "Shirt" Nothing [] $ HashMap.fromList
    [ ("size", sizeFieldType)
    ]

hatType :: Out.ObjectType IO
hatType = Out.ObjectType "Hat" Nothing [] $ HashMap.fromList
    [ ("size", sizeFieldType)
    , ("circumference", circumferenceFieldType)
    ]

circumferenceFieldType :: Out.Resolver IO
circumferenceFieldType
    = Out.ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
    $ pure $ snd circumference

sizeFieldType :: Out.Resolver IO
sizeFieldType
    = Out.ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
    $ pure $ snd size

toSchema :: Text -> (Text, Value) -> Schema IO
toSchema t (_, resolve) = schema queryType Nothing Nothing mempty
  where
    garmentType = Out.UnionType "Garment" Nothing [hatType, shirtType]
    typeNameField = Out.Field Nothing (Out.NamedScalarType string) mempty
    garmentField = Out.Field Nothing (Out.NamedUnionType garmentType) mempty
    queryType =
        case t of
            "circumference" -> hatType
            "size" -> shirtType
            _ -> Out.ObjectType "Query" Nothing []
                $ HashMap.fromList
                    [ ("garment", ValueResolver garmentField (pure resolve))
                    , ("__typename", ValueResolver typeNameField (pure $ String "Shirt"))
                    ]

spec :: Spec
spec = do
    describe "Inline fragment executor" $ do
        it "chooses the first selection if the type matches" $ do
            let localSchema = toSchema "Hat" $ garment "Hat"
            actual <- GraphQL.graphql localSchema Nothing (mempty :: HashMap Name Value) inlineQuery
            let expected = Object
                    $ HashMap.singleton "garment"
                    $ Object
                    $ HashMap.singleton  "circumference"
                    $ Int 60
             in actual `shouldResolveTo` expected

        it "chooses the last selection if the type matches" $ do
            let localSchema = toSchema "Shirt" $ garment "Shirt"
            actual <- GraphQL.graphql localSchema Nothing (mempty :: HashMap Name Value) inlineQuery
            let expected = Object
                    $ HashMap.singleton "garment"
                    $ Object
                    $ HashMap.singleton "size"
                    $ String "L"
             in actual `shouldResolveTo` expected

        it "embeds inline fragments without type" $ do
            let sourceQuery = [gql|
              {
                circumference
                ... {
                  size
                }
              }
            |]
            let localSchema = toSchema "circumference" circumference
            actual <- GraphQL.graphql localSchema Nothing (mempty :: HashMap Name Value) sourceQuery
            let expected = Object $ HashMap.fromList
                    [ ("circumference", Int 60)
                    , ("size", String  "L")
                    ]
             in actual `shouldResolveTo` expected

        it "evaluates fragments on Query" $ do
            let sourceQuery = [gql|
              {
                ... {
                  size
                }
              }
            |]
                localSchema = toSchema "size" size
                actual :: Text -> IO (Either (ResponseEventStream IO Value) (Response Value))
                actual = GraphQL.graphql localSchema Nothing (mempty :: HashMap Name Value)
             in actual `shouldResolve` sourceQuery

    describe "Fragment spread executor" $ do
        it "evaluates fragment spreads" $ do
            let sourceQuery = [gql|
              {
                ...circumferenceFragment
              }

              fragment circumferenceFragment on Hat {
                circumference
              }
            |]
            let localSchema = toSchema "circumference" circumference
            actual <- GraphQL.graphql localSchema Nothing (mempty :: HashMap Name Value) sourceQuery
            let expected = Object
                    $ HashMap.singleton "circumference"
                    $ Int 60
             in actual `shouldResolveTo` expected

        it "evaluates nested fragments" $ do
            let sourceQuery = [gql|
              {
                garment {
                  ...circumferenceFragment
                }
              }

              fragment circumferenceFragment on Hat {
                ...hatFragment
              }

              fragment hatFragment on Hat {
                circumference
              }
            |]
            let localSchema = toSchema "Hat" $ garment "Hat"
            actual <- GraphQL.graphql localSchema Nothing (mempty :: HashMap Name Value) sourceQuery
            let expected = Object
                    $ HashMap.singleton "garment"
                    $ Object
                    $ HashMap.singleton "circumference"
                    $ Int 60
             in actual `shouldResolveTo` expected

        it "considers type condition" $ do
            let sourceQuery = [gql|
              {
                garment {
                  ...circumferenceFragment
                  ...sizeFragment
                }
              }
              fragment circumferenceFragment on Hat {
                circumference
              }
              fragment sizeFragment on Shirt {
                size
              }
            |]
                expected = Object
                    $ HashMap.singleton "garment"
                    $ Object
                    $ HashMap.singleton "circumference"
                    $ Int 60
            let localSchema = toSchema "Hat" $ garment "Hat"
            actual <- GraphQL.graphql localSchema Nothing (mempty :: HashMap Name Value) sourceQuery
            actual `shouldResolveTo` expected
