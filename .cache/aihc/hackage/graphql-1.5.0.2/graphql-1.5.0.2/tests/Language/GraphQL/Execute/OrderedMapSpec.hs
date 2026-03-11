{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Execute.OrderedMapSpec
    ( spec
    ) where

import Language.GraphQL.Execute.OrderedMap (OrderedMap)
import qualified Language.GraphQL.Execute.OrderedMap as OrderedMap
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
    describe "OrderedMap" $ do
        it "creates an empty map" $
            (mempty :: OrderedMap String) `shouldSatisfy` null

        it "creates a singleton" $
            let value :: String
                value = "value"
             in OrderedMap.size (OrderedMap.singleton "key" value) `shouldBe` 1

        it "combines inserted vales" $
            let key = "key"
                map1 = OrderedMap.singleton key ("1" :: String)
                map2 = OrderedMap.singleton key ("2" :: String)
             in OrderedMap.lookup key (map1 <> map2) `shouldBe` Just "12"

        it "shows the map" $
            let actual = show
                    $ OrderedMap.insert "key1" "1"
                    $ OrderedMap.singleton "key2" ("2" :: String)
                expected = "fromList [(\"key2\",\"2\"),(\"key1\",\"1\")]"
             in actual `shouldBe` expected

        it "traverses a map of just values" $
            let actual = sequence
                    $ OrderedMap.insert "key1" (Just "2")
                    $ OrderedMap.singleton "key2" $ Just ("1" :: String)
                expected = Just
                    $ OrderedMap.insert "key1" "2"
                    $ OrderedMap.singleton "key2" ("1" :: String)
             in actual `shouldBe` expected

        it "traverses a map with a Nothing" $
            let actual = sequence
                    $ OrderedMap.insert "key1" Nothing
                    $ OrderedMap.singleton "key2" $ Just ("1" :: String)
                expected = Nothing
             in actual `shouldBe` expected

        it "combines two maps preserving the order of the second one" $
            let map1 :: OrderedMap String
                map1 = OrderedMap.insert "key2" "2"
                    $ OrderedMap.singleton "key1" "1"
                map2 :: OrderedMap String
                map2 = OrderedMap.insert "key4" "4"
                    $ OrderedMap.singleton "key3" "3"
                expected = OrderedMap.insert "key4" "4"
                    $ OrderedMap.insert "key3" "3"
                    $ OrderedMap.insert "key2" "2"
                    $ OrderedMap.singleton "key1" "1"
             in (map1 <> map2) `shouldBe` expected

        it "replaces existing values" $
            let key = "key"
                actual = OrderedMap.replace key ("2" :: String)
                        $ OrderedMap.singleton key ("1" :: String)
             in OrderedMap.lookup key actual `shouldBe` Just "2"
