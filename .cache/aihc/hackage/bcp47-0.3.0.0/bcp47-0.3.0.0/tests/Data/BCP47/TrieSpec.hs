{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.BCP47.TrieSpec
  ( spec
  ) where

import TestImport hiding (lookup)

import Data.BCP47
import Data.BCP47.Trie
import Data.Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T

catMaybes :: Trie (Maybe a) -> Maybe (Trie a)
catMaybes = mapMaybe id

spec :: Spec
spec = do
  describe "Trie" $ do
    it "has equality" $ property $ \xs ->
      fromList xs `shouldBe` (fromList xs :: Maybe (Trie Bool))

    it "can be ordered"
      $ singleton en "color"
      < singleton es "color"
      `shouldBe` True

    describe "Semigroup instance" $ do
      it "unions top-level conflicts" $ property $ \l r ->
        lookup en (singleton en [l] <> singleton en [r])
          `shouldBe` Just ([l, r] :: [Int])

      it "unions leaf-level conflicts" $ property $ \v1 v2 -> do
        lookup enGB (singleton enGB [v1] <> singleton enGB [v2])
          `shouldBe` Just ([v1, v2] :: [Int])

    describe "mapMaybe" $ do
      it "Justs are constant" $ property $ \xs ->
        let
          trie = fromList xs :: Maybe (Trie (Maybe Bool))
          expected = List.sort <$> do
            m <- Maybe.catMaybes . toList <$> trie
            if null m then Nothing else Just m
          actual = List.sort . toList <$> (catMaybes =<< trie)
        in expected == actual

      it "returns Nothing if empty resulting Trie" $ do
        let
          (Just given) = fromList [(en, Nothing), (enGB, Nothing)]
          expected = fromList @String []
        catMaybes given `shouldBe` expected

      it "returns top-level Just" $ do
        let
          (Just given) = fromList [(en, Just "color"), (enGB, Nothing)]
          expected = fromList [(en, "color")]
        catMaybes given `shouldBe` expected

      it "returns leaf Just" $ do
        let
          (Just given) = fromList [(en, Nothing), (enGB, Just "colour")]
          expected = fromList [(enGB, "colour")]
        catMaybes given `shouldBe` expected

      it "returns both leaf and top-level Justs" $ do
        let
          (Just given) = fromList [(en, Just "color"), (enGB, Just "colour")]
          expected = fromList [(en, "color"), (enGB, "colour")]
        catMaybes given `shouldBe` expected

  describe "lookup" $ do
    it "should always lookup a path it inserts" $ property $ \tag ->
      lookup tag (singleton tag "string") `shouldBe` Just "string"

    it "lookups no match" $ do
      let Just trie = fromList [(en, "color"), (enGB, "colour")]
      lookup es trie `shouldBe` Nothing

    it "lookups no match deeply" $ do
      let Just trie = fromList [(enGBTJP, "colour")]
      lookup enGB trie `shouldBe` Nothing

    it "lookups an exact match" $ do
      let Just trie = fromList [(en, "color"), (enGB, "colour")]
      lookup en trie `shouldBe` Just "color"

    it "lookups on just language" $ do
      let Just trie = fromList [(en, "color"), (es, "colour")]
      lookup es trie `shouldBe` Just "colour"

    it "lookups a deep exact match" $ do
      let Just trie = fromList [(enGBTJP, "foo"), (enGB, "colour")]
      lookup enGBTJP trie `shouldBe` Just "foo"

    it "lookups a relevant match" $ do
      let Just trie = fromList [(en, "color"), (enGB, "colour")]
      lookup enTJP trie `shouldBe` Just "color"

    it "lookups a deep relevant match" $ do
      let Just trie = fromList [(en, "color"), (enGB, "colour")]
      lookup enGBTJP trie `shouldBe` Just "colour"

    it "lookups case-insensitively" $ property $ \tag -> do
      let tagText = toText tag
      let Right upper = fromText $ T.toUpper tagText
      let Right lower = fromText $ T.toLower tagText
      let trie = singleton upper "color"
      lookup lower trie `shouldBe` Just "color"

  describe "match" $ do
    it "should always match a path it inserts" $ property $ \tag ->
      match tag (singleton tag "string") `shouldBe` Just "string"

    it "matches no match" $ do
      let Just trie = fromList [(en, "color"), (enGB, "colour")]
      match es trie `shouldBe` Nothing

    it "matches no match deeply" $ do
      let Just trie = fromList [(enGBTJP, "colour")]
      match enGB trie `shouldBe` Nothing

    it "matches an exact match" $ do
      let Just trie = fromList [(en, "color"), (enGB, "colour")]
      match en trie `shouldBe` Just "color"

    it "matches on just language" $ do
      let Just trie = fromList [(en, "color"), (es, "colour")]
      match es trie `shouldBe` Just "colour"

    it "matches a deep exact match" $ do
      let Just trie = fromList [(enGBTJP, "foo"), (enGB, "colour")]
      match enGBTJP trie `shouldBe` Just "foo"

    it "matches a relevant match" $ do
      let Just trie = fromList [(en, "color"), (enGB, "colour")]
      match enTJP trie `shouldBe` Nothing

    it "matches a deep relevant match" $ do
      let Just trie = fromList [(en, "color"), (enGB, "colour")]
      match enGBTJP trie `shouldBe` Nothing

    it "matches case-insensitively" $ do
      let Just trie = fromList [(enTJPUpper, "color"), (enGBTJPUpper, "color")]
      match enTJP trie `shouldBe` Just "color"
