{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.DirectiveSpec
    ( spec
    ) where

import Language.GraphQL.AST.Document (Name)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Language.GraphQL as GraphQL
import Language.GraphQL.TH
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out
import Test.Hspec (Spec, describe, it)
import Test.Hspec.GraphQL

experimentalResolver :: Schema IO
experimentalResolver = schema queryType Nothing Nothing mempty
  where
    queryType = Out.ObjectType "Query" Nothing []
        $ HashMap.singleton "experimentalField"
        $ Out.ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
        $ pure $ Int 5

spec :: Spec
spec =
    describe "Directive executor" $ do
        it "should be able to @skip fields" $ do
            let sourceQuery = [gql|
              {
                experimentalField @skip(if: true)
              }
            |]

            actual <- GraphQL.graphql experimentalResolver Nothing (mempty :: HashMap Name Value) sourceQuery
            actual `shouldResolveTo` Object mempty

        it "should not skip fields if @skip is false" $ do
            let sourceQuery = [gql|
              {
                experimentalField @skip(if: false)
              }
            |]
                expected = Object $ HashMap.singleton "experimentalField" (Int 5)
            actual <- GraphQL.graphql experimentalResolver Nothing (mempty :: HashMap Name Value) sourceQuery
            actual `shouldResolveTo` expected

        it "should skip fields if @include is false" $ do
            let sourceQuery = [gql|
              {
                experimentalField @include(if: false)
              }
            |]

            actual <- GraphQL.graphql experimentalResolver Nothing (mempty :: HashMap Name Value) sourceQuery
            actual `shouldResolveTo` Object mempty

        it "should be able to @skip a fragment spread" $ do
            let sourceQuery = [gql|
              {
                ...experimentalFragment @skip(if: true)
              }

              fragment experimentalFragment on Query {
                experimentalField
              }
            |]

            actual <- GraphQL.graphql experimentalResolver Nothing (mempty :: HashMap Name Value) sourceQuery
            actual `shouldResolveTo` Object mempty

        it "should be able to @skip an inline fragment" $ do
            let sourceQuery = [gql|
              {
                ... on Query @skip(if: true) {
                  experimentalField
                }
              }
            |]

            actual <- GraphQL.graphql experimentalResolver Nothing (mempty :: HashMap Name Value) sourceQuery
            actual `shouldResolveTo` Object mempty
