{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.AST.DocumentSpec
    ( spec
    ) where

import Language.GraphQL.AST.Document
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "Document" $ do
        it "shows objects" $
            let zero = Location 0 0
                object = ConstObject
                    [ ObjectField "field1" (Node (ConstFloat 1.2) zero) zero
                    , ObjectField "field2" (Node ConstNull zero) zero
                    ]
                expected = "{ field1: 1.2, field2: null }"
             in show object `shouldBe` expected

    describe "Description" $
        it "keeps content when merging with no description" $
            let expected = Description $ Just "Left description"
                actual = expected <> Description Nothing
             in actual `shouldBe` expected
