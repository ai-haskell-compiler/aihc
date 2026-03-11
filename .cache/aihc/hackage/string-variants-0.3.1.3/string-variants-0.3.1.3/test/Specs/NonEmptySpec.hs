{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Specs.NonEmptySpec (spec) where

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.StringVariants.NonEmptyText
import Data.StringVariants.NullableNonEmptyText
import Data.StringVariants.Util (usePositiveNat)
import GHC.TypeLits
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Haskell.TH.Quote (QuasiQuoter (quoteExp))
import Prelude
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

mkNonEmptyText299 :: Text -> Maybe (NonEmptyText 299)
mkNonEmptyText299 = mkNonEmptyText

maybeTextToTruncateNullableNonEmptyText299 :: Maybe Text -> NullableNonEmptyText 299
maybeTextToTruncateNullableNonEmptyText299 = maybeTextToTruncateNullableNonEmptyText

mkNonEmptyText299WithTruncate :: Text -> Maybe (NonEmptyText 299)
mkNonEmptyText299WithTruncate = mkNonEmptyTextWithTruncate

matchLength :: proxy (n :: Nat) -> other n -> other n
matchLength _ = id

spec :: Spec
spec = describe "NonEmptyText variants" $ do
  describe "NonEmptyText299" $ do
    describe "compileNonEmptyText" $ do
      it "should work" $ do
        mkNonEmptyText299 "hi" `shouldBe` Just (widen [compileNonEmptyTextKnownLength|hi|])
    describe "compileNullableNonEmptyText" $ do
      it "should work" $ do
        $(quoteExp (compileNullableNonEmptyText 2) "yo") `shouldBe` NullableNonEmptyText (Just [compileNonEmptyTextKnownLength|yo|])
    describe "nonEmptyTextToText" $ do
      it "should work" $ do
        nonEmptyTextToText [compileNonEmptyTextKnownLength|yo|] `shouldBe` "yo"

  describe "Generalized NonEmptyText" $ do
    describe "mkNonEmptyText" $
      it "should work" $ do
        mkNonEmptyText299 "" `shouldBe` Nothing
        mkNonEmptyText299 " " `shouldBe` Nothing
        mkNonEmptyText299 "\n" `shouldBe` Nothing
        mkNonEmptyText299 "\t" `shouldBe` Nothing
        mkNonEmptyText299 "\NUL" `shouldBe` Nothing
        mkNonEmptyText299 "x" `shouldSatisfy` isJust

    describe "literalNonEmptyText" $ do
      it "should work" $ do
        Just (literalNonEmptyText @"abc def") `shouldBe` mkNonEmptyText299 "abc def"

-- test cases for bad strings. Alas, the -fdefer-type-errors defers the errors but then happily creates invalid values in runtime. Is this a GHC bug?

      -- describe "rejects invalid strings" $ do
      --   it "rejects string too long" $ do
      --     print (literalNonEmptyText @"abcdefghijkl" :: NonEmptyText 10)
      --       `shouldThrow` (("Invalid NonEmptyText. Needs to be <= 10 characters. Has 12 characters." `isInfixOf`) . show)
      --   it "rejects empty string" $ do
      --     print (literalNonEmptyText @"" :: NonEmptyText 10)
      --       `shouldThrow` (("Symbol is empty" `isInfixOf`) . show)
      --   it "rejects string with leading whitespace" $ do
      --     print (literalNonEmptyText @" abc" :: NonEmptyText 10)
      --       `shouldThrow` (("Symbol has leading whitespace" `isInfixOf`) . show)
      --   it "rejects string with trailing whitespace" $ do
      --     print (literalNonEmptyText @"abc " :: NonEmptyText 10)
      --       `shouldThrow` (("Symbol has leading whitespace" `isInfixOf`) . show)
      --   it "rejects string with leading unicode whitespace" $ do
      --     print (literalNonEmptyText @"\x2000abc" :: NonEmptyText 10)
      --       `shouldThrow` (("Symbol has leading whitespace" `isInfixOf`) . show)

    describe "maybeTextToTruncateNullableNonEmptyText" $ do
      it "common behavior" $ do
        maybeTextToTruncateNullableNonEmptyText299 Nothing `shouldBe` NullableNonEmptyText Nothing
        maybeTextToTruncateNullableNonEmptyText299 (Just "") `shouldBe` NullableNonEmptyText Nothing
        maybeTextToTruncateNullableNonEmptyText299 (Just " ") `shouldBe` NullableNonEmptyText Nothing
      it "type-dependent behavior" $
        hedgehog $ do
          n <- forAll $ Gen.integral $ Range.constant 1 20000
          let n' = fromInteger n
          usePositiveNat n (pure ()) $ \p -> do
            let NullableNonEmptyText mtext =
                  matchLength p $
                    maybeTextToTruncateNullableNonEmptyText
                      (Just $ T.pack $ replicate (n' + 1) 'x')
            (T.length . nonEmptyTextToText <$> mtext) === Just n'

    describe "mkNonEmptyTextWithTruncate" $ do
      it "common behavior" $ do
        mkNonEmptyText299WithTruncate "" `shouldBe` Nothing
        mkNonEmptyText299WithTruncate " " `shouldBe` Nothing
      it "type-dependent behavior" $
        hedgehog $ do
          n <- forAll $ Gen.integral $ Range.constant 1 20000
          let n' = fromInteger n
          usePositiveNat n (pure ()) $ \(_ :: proxy n) -> do
            let mtext = mkNonEmptyTextWithTruncate @n (T.pack $ replicate (n' + 1) 'x')
            (T.length . nonEmptyTextToText <$> mtext) === Just n'

    describe "mkNullableNonEmptyText" $ do
      it "should reject strings that are too big" $
        mkNullableNonEmptyText @2 "hey" `shouldBe` Nothing
      it "should accept empty strings" $
        mkNullableNonEmptyText @2 "" `shouldBe` Just (NullableNonEmptyText Nothing)
      it "should accept whitespace strings" $
        mkNullableNonEmptyText @2 " " `shouldBe` Just (NullableNonEmptyText Nothing)
      it "should strip whitespace" $
        mkNullableNonEmptyText @2 " hi " `shouldBe` Just (NullableNonEmptyText $ Just $ unsafeMkNonEmptyText "hi")
