{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.THSpec
    ( spec
    ) where

import Data.Text (Text)
import Language.GraphQL.TH (gql)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "gql" $
        it "replaces CRNL with NL" $
            let expected :: Text
                expected = "line1\nline2\nline3"
                actual = [gql|
                  line1
                  line2
                  line3
                |]
             in actual `shouldBe` expected
