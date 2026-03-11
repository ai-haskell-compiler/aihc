{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.RootOperationSpec
    ( spec
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL
import Language.GraphQL.AST (Name)
import Test.Hspec (Spec, describe, it)
import Language.GraphQL.TH
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out
import Test.Hspec.GraphQL

hatType :: Out.ObjectType IO
hatType = Out.ObjectType "Hat" Nothing []
    $ HashMap.singleton "circumference"
    $ ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
    $ pure $ Int 60

garmentSchema :: Schema IO
garmentSchema = schema queryType (Just mutationType) Nothing mempty
  where
    queryType = Out.ObjectType "Query" Nothing [] hatFieldResolver
    mutationType = Out.ObjectType "Mutation" Nothing [] incrementFieldResolver
    garment = pure $ Object $ HashMap.fromList
        [ ("circumference", Int 60)
        ]
    incrementFieldResolver = HashMap.singleton "incrementCircumference"
        $ ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
        $ pure $ Int 61
    hatField = Out.Field Nothing (Out.NamedObjectType hatType) mempty
    hatFieldResolver =
        HashMap.singleton "garment" $ ValueResolver hatField garment

spec :: Spec
spec =
    describe "Root operation type" $ do
        it "returns objects from the root resolvers" $ do
            let querySource = [gql|
              {
                garment {
                  circumference
                }
              }
            |]
                expected = Object
                    $ HashMap.singleton "garment"
                    $ Object
                    $ HashMap.singleton "circumference"
                    $ Int 60
            actual <- graphql garmentSchema Nothing (mempty :: HashMap Name Value) querySource
            actual `shouldResolveTo` expected

        it "chooses Mutation" $ do
            let querySource = [gql|
              mutation {
                incrementCircumference
              }
            |]
                expected = Object
                    $ HashMap.singleton "incrementCircumference"
                    $ Int 61
            actual <- graphql garmentSchema Nothing (mempty :: HashMap Name Value) querySource
            actual `shouldResolveTo` expected
