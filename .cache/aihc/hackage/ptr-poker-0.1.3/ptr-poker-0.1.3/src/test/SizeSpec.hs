{-# OPTIONS_GHC -Wno-missing-signatures #-}

module SizeSpec (spec) where

import qualified Data.ByteString.Char8 as Char8ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified PtrPoker.Size as Size
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Size" $ do
    describe "word64AsciiDec" $ do
      it "computes correct size for Word64 ASCII decimal representation"
        $ property
        $ \a -> Size.word64AsciiDec a === length (show a)

    describe "int64AsciiDec" $ do
      it "computes correct size for Int64 ASCII decimal representation"
        $ property
        $ \a -> Size.int64AsciiDec a === length (show a)

    describe "textUtf8" $ do
      it "computes correct size for Text UTF-8 encoding (with unicode)"
        $ property
        $ \(NonEmpty str) ->
          let text = Text.pack str
           in Size.textUtf8 text === Char8ByteString.length (TextEncoding.encodeUtf8 text)

      it "computes correct size for Text UTF-8 encoding (ASCII only)"
        $ property
        $ forAll (getNonEmpty <$> (arbitrary `suchThat` (all isAsciiChar . getNonEmpty)))
        $ \str ->
          let text = Text.pack str
           in Size.textUtf8 text === Char8ByteString.length (TextEncoding.encodeUtf8 text)
  where
    isAsciiChar c = c <= '\127'
