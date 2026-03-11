{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.GraphQL.ClassSpec
    ( spec
    ) where

import Data.Text (Text)
import Data.Time (UTCTime(..))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import qualified Language.GraphQL.Type as Type
import Language.GraphQL.Class
    ( FromGraphQL(..)
    , ToGraphQL(..)
    , deriveFromGraphQL
    , deriveToGraphQL
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.HashMap.Strict as HashMap

data TwoFieldRecord = TwoFieldRecord
    { x :: Int
    , y :: Bool
    } deriving (Eq, Show)

$(deriveToGraphQL ''TwoFieldRecord)
$(deriveFromGraphQL ''TwoFieldRecord)

data TwoVariantUnion
    = FirstVariantUnion TwoFieldRecord
    | SecondVariantUnion TwoFieldRecord
    deriving (Eq, Show)

$(deriveToGraphQL ''TwoVariantUnion)

newtype NewTypeRecord = NewTypeRecord { newTypeField :: Int }
    deriving (Eq, Show)

$(deriveToGraphQL ''NewTypeRecord)
$(deriveFromGraphQL ''NewTypeRecord)

data TwoFieldEnum = TWO_FIELD_ENUM_1 | TWO_FIELD_ENUM_2
    deriving (Eq, Show)

$(deriveToGraphQL ''TwoFieldEnum)
$(deriveFromGraphQL ''TwoFieldEnum)

spec :: Spec
spec = do
    describe "ToGraphQL" $ do
        it "converts integers" $
            toGraphQL (5 :: Int) `shouldBe` Type.Int 5

        it "converts text" $
            toGraphQL ("String" :: Text) `shouldBe` Type.String "String"

        it "converts booleans" $
            toGraphQL True `shouldBe` Type.Boolean True

        it "converts Nothing to Null" $
            toGraphQL (Nothing :: Maybe Int) `shouldBe` Type.Null

        it "converts singleton lists" $
            toGraphQL [True] `shouldBe` Type.List [Type.Boolean True]

        it "converts UTCTime" $
            let given = UTCTime
                    { utctDay = fromOrdinalDate 2023 5
                    , utctDayTime = 90
                    }
                actual = toGraphQL given
                expected = Type.String "2023-01-05T00:01:30Z"
             in actual `shouldBe` expected

    describe "FromGraphQL" $ do
        it "converts integers" $
            fromGraphQL (Type.Int 5) `shouldBe` Just (5 :: Int)

        it "converts text" $
            fromGraphQL (Type.String "String") `shouldBe` Just ("String" :: Text)

        it "converts booleans" $
            fromGraphQL (Type.Boolean True) `shouldBe` Just True

        it "converts Null to Nothing" $
            fromGraphQL Type.Null `shouldBe` Just (Nothing :: Maybe Int)

        it "converts singleton lists" $
            fromGraphQL (Type.List [Type.Boolean True]) `shouldBe` Just [True]

        it "converts UTCTime" $
            let given = Type.String "2023-01-05T00:01:30Z"
                expected = Just $ UTCTime
                    { utctDay = fromOrdinalDate 2023 5
                    , utctDayTime = 90
                    }
                actual = fromGraphQL given
             in actual `shouldBe` expected

    describe "deriveToGraphQL" $ do
        it "derives ToGraphQL for a record with multiple fields" $
            let expected = Type.Object $ HashMap.fromList
                    [ ("x", Type.Int 1)
                    , ("y", Type.Boolean True)
                    , ("__typename", Type.String "TwoFieldRecord")
                    ]
                given = TwoFieldRecord
                    { x = 1
                    , y = True
                    }
             in toGraphQL given `shouldBe` expected

        it "derives ToGraphQL for a union" $
            let expected = Type.Object $ HashMap.fromList
                    [ ("x", Type.Int 2)
                    , ("y", Type.Boolean False)
                    , ("__typename", Type.String "TwoFieldRecord")
                    ]
                given = SecondVariantUnion $ TwoFieldRecord
                    { x = 2
                    , y = False
                    }
             in toGraphQL given `shouldBe` expected

        it "derives ToGraphQL for a newtype record" $
            let expected = Type.Object $ HashMap.fromList
                    [ ("newTypeField", Type.Int 3)
                    , ("__typename", Type.String "NewTypeRecord")
                    ]
                given = NewTypeRecord 3
             in toGraphQL given `shouldBe` expected

        it "derives ToGraphQL for an enumeration" $
            let expected = Type.Enum "TWO_FIELD_ENUM_2"
                given = TWO_FIELD_ENUM_2
             in toGraphQL given `shouldBe` expected

    describe "deriveFromGraphQL" $ do
        it "derives FromGraphQL for a record with multiple fields" $
            let given = Type.Object $ HashMap.fromList
                    [ ("x", Type.Int 1)
                    , ("y", Type.Boolean True)
                    ]
                expected = TwoFieldRecord
                    { x = 1
                    , y = True
                    }
             in fromGraphQL given `shouldBe` Just expected

        it "derives FromGraphQL for a newtype record" $
            let given = Type.Object $ HashMap.singleton "newTypeField" (Type.Int 3)
                expected = NewTypeRecord 3
             in fromGraphQL given `shouldBe` Just expected

        it "derives FromGraphQL for an enumeration" $
            let given = Type.Enum "TWO_FIELD_ENUM_2"
                expected = TWO_FIELD_ENUM_2
             in fromGraphQL given `shouldBe` Just expected
