{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.Execute.CoerceSpec
    ( spec
    ) where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isNothing)
import qualified Language.GraphQL.Execute.Coerce as Coerce
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.In as In
import Prelude hiding (id)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

direction :: EnumType
direction = EnumType "Direction" Nothing  $ HashMap.fromList
    [ ("NORTH", EnumValue Nothing)
    , ("EAST", EnumValue Nothing)
    , ("SOUTH", EnumValue Nothing)
    , ("WEST", EnumValue Nothing)
    ]

namedIdType :: In.Type
namedIdType = In.NamedScalarType id

spec :: Spec
spec =
    describe "coerceInputLiteral" $ do
        it "coerces enums" $
            let expected = Just (Enum "NORTH")
                actual = Coerce.coerceInputLiteral
                    (In.NamedEnumType direction) (Enum "NORTH")
             in actual `shouldBe` expected
        it "fails with non-existing enum value" $
            let actual = Coerce.coerceInputLiteral
                    (In.NamedEnumType direction) (Enum "NORTH_EAST")
             in actual `shouldSatisfy` isNothing
        it "coerces integers to IDs" $
            let expected = Just (String "1234")
                actual = Coerce.coerceInputLiteral namedIdType (Int 1234)
             in actual `shouldBe` expected
        it "coerces nulls" $ do
            let actual = Coerce.coerceInputLiteral namedIdType Null
             in actual `shouldBe` Just Null
        it "wraps singleton lists" $ do
            let expected = Just $ List [List [String "1"]]
                embeddedType = In.ListType $ In.ListType namedIdType
                actual = Coerce.coerceInputLiteral embeddedType (String "1")
             in actual `shouldBe` expected
