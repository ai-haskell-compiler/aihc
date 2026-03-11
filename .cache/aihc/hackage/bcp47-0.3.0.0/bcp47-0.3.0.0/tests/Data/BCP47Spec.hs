{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.BCP47Spec
  ( spec
  ) where

import TestImport

import Country.Identifier (china)
import Data.BCP47
import Data.BCP47.Internal.Extension
import Data.BCP47.Internal.LanguageExtension
import Data.BCP47.Internal.PrivateUse
import Data.BCP47.Internal.Script
import Data.BCP47.Internal.Variant
import Data.Either (isRight)
import Data.LanguageCodes (ISO639_1 (ZH))
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "fromText" $ do
    it "parses all components" $ do
      lng <-
        fromTextThrows
          "zh-abc-def-zxy-Hant-CN-1967-y-extensi-x-private1-private2"
      language lng `shouldBe` ZH
      extendedLanguageSubtags lng
        `shouldBe` Set.singleton (LanguageExtension "abc-def-zxy")
      script lng `shouldBe` Just (Script "Hant")
      region lng `shouldBe` Just china
      variants lng `shouldBe` Set.singleton (Variant "1967")
      extensions lng `shouldBe` Set.singleton (Extension "y-extensi")
      privateUse lng
        `shouldBe` Set.fromList [PrivateUse "private1", PrivateUse "private2"]

    it "only parses complete subtags" $ do
      -- Specifically, the region CN should not be parsed out of the variant CNUVWXYX
      lng <- fromTextThrows "zh-CNUVWXYX"
      language lng `shouldBe` ZH
      extendedLanguageSubtags lng `shouldBe` Set.empty
      script lng `shouldBe` Nothing
      region lng `shouldBe` Nothing
      variants lng `shouldBe` Set.singleton (Variant "CNUVWXYX")
      privateUse lng `shouldBe` Set.empty

  describe "Arbitrary"
    . it "can parse arbitrary generated tags"
    . property
    $ \tag -> fromText (toText tag) `shouldBe` Right tag

  describe "Read/Show" . it "can roundtrip" . property $ \tag ->
    readMaybe (show @BCP47 tag) `shouldBe` Just tag

  describe "Eq" $ do
    it "compares equal with different casing" $ do
      lower <-
        fromTextThrows
          "zh-abc-def-zxy-hant-cn-1967-y-extensi-x-private1-private2"
      upper <-
        fromTextThrows
          "ZH-ABC-DEF-ZXY-HANT-CN-1967-Y-EXTENSI-X-PRIVATE1-PRIVATE2"
      upper `shouldBe` lower

  describe "Regression tests" $ do
    -- We used to require Region to be uppercase (incorrectly)
    it "accepts en-gb" $ fromText "en-gb" `shouldBe` Right enGB

  describe "Known bugs" $ do
    -- Per https://datatracker.ietf.org/doc/html/rfc5646#section-2.2.4
    -- we should accept e.g. 419 (Latin America and the Caribbean), but
    -- not 3-digit M49 codes for regions that already have a 2-letter
    -- code, e.g. 012 (Algeria, DZ)
    --
    -- The country library we use only recognizes 3-digit M49 codes for
    -- countries, so it rejects other kinds of regions.
    it "accepts es-419" $ do
      pendingWith "country library does not recognize non-country M49 codes"
      let result = fromText "es-419"
      result `shouldSatisfy` isRight

    -- Per https://www.iso.org/obp/ui/#iso:code:3166:TP
    -- East Timor changed to Timor-Leste, but TP is still reserved
    -- until 2052-05.
    --
    -- The country library we use only recognizes Timor-Leste (TL),
    -- so it rejects TP.
    it "accepts en-TP" $ do
      pendingWith "country library does not recognize TP anymore"
      let result = fromText "es-TP"
      result `shouldSatisfy` isRight

    -- Per https://datatracker.ietf.org/doc/html/rfc5646#section-2.2.2
    -- Languages encompassed by certain macrolanguages can be
    -- registered as extended language subtags. These may be represented
    -- either with the lone subtag for the encompassed language, e.g.
    -- cmn for Mandarin, or with a language-extlang combination, e.g.
    -- zh-cmn. The first option is preferred.
    --
    -- We currently don't support this because we always expect a
    -- leading ISO-639-1 code.
    it "accepts cnm" $ do
      pendingWith "Parser incorrectly requires a leading ISO-639-1 code"
      let result = fromText "cnm"
      result `shouldSatisfy` isRight
