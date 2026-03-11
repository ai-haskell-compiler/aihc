{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | /Human beings on our planet have, past and present, used a number of/
-- /languages. There are many reasons why one would want to identify the/
-- /language used when presenting or requesting information./
--
-- /The language of an information item or a user's language preferences often/
-- /need to be identified so that appropriate processing can be applied. For/
-- /example, the user's language preferences in a Web browser can be used to/
-- /select Web pages appropriately. Language information can also be used to/
-- /select among tools (such as dictionaries) to assist in the processing or/
-- /understanding of content in different languages.  Knowledge about the/
-- /particular language used by some piece of information content might be useful/
-- /or even required by some types of processing, for example, spell-checking,/
-- /computer-synthesized speech, Braille transcription, or high-quality print/
-- /renderings./
--
-- / - /<https://tools.ietf.org/html/bcp47>
module Data.BCP47
  ( BCP47
  , inits

    -- * Construction
  , mkLanguage
  , mkLocalized
  , fromText
  , parser

    -- * Serialization
  , toText

    -- * Subtags

    -- | A language tag is composed from a sequence of one or more "subtags",
    -- each of which refines or narrows the range of language identified by
    -- the overall tag. Subtags, in turn, are a sequence of alphanumeric characters
    -- (letters and digits), distinguished and separated from other subtags in a tag
    -- by a hyphen ("-", [Unicode] U+002D).
  , toSubtags

    -- ** Language
  , ISO639_1
  , language
  , languageToText
  , languageFromText

    -- ** Language Extension
  , LanguageExtension
  , extendedLanguageSubtags
  , languageExtensionToText
  , languageExtensionFromText

    -- ** Language Script
  , Script
  , script
  , scriptToText
  , scriptFromText

    -- ** Region
  , Country
  , region
  , regionToText
  , regionFromText

    -- ** Variant
  , Variant
  , variants
  , variantToText
  , variantFromText

    -- ** Extension
  , Extension
  , extensions
  , extensionToText
  , extensionFromText

    -- ** Private Use
  , PrivateUse
  , privateUse
  , privateUseToText
  , privateUseFromText

    -- * For testing
  , en
  , es
  , sw
  , enGB
  , enUS
  )
where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus)
import Country.Identifier
  ( unitedKingdomOfGreatBritainAndNorthernIreland
  , unitedStatesOfAmerica
  )
import Data.BCP47.Internal.Arbitrary
  ( Arbitrary
  , arbitrary
  , choose
  , elements
  , listOf
  , vectorOf
  )
import Data.BCP47.Internal.Extension
import Data.BCP47.Internal.Language
import Data.BCP47.Internal.LanguageExtension
import Data.BCP47.Internal.PrivateUse
import Data.BCP47.Internal.Region
import Data.BCP47.Internal.Script
import Data.BCP47.Internal.Subtags
import Data.BCP47.Internal.Variant
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.LanguageCodes (ISO639_1 (EN, ES, SW))
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, hidden, many, optional, parse, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

-- | A language tag
--
-- Language tags are used to help identify languages, whether spoken, written,
-- signed, or otherwise signaled, for the purpose of communication. This
-- includes constructed and artificial languages but excludes languages not
-- intended primarily for human communication, such as programming languages.
data BCP47 = BCP47
  { language :: ISO639_1
  -- ^ The language subtag
  , subtags :: Set Subtags
  }
  deriving stock (Eq, Ord)

instance Arbitrary BCP47 where
  arbitrary = BCP47 <$> elements [EN, ES] <*> specs
   where
    oneOrNone f = choose (0, 1) >>= (`vectorOf` (f <$> arbitrary))
    manyOf f = listOf (f <$> arbitrary)
    regions = [minBound .. maxBound]
    specs =
      Set.fromList . mconcat
        <$> sequenceA
          [ manyOf SpecifyLanguageExtension
          , oneOrNone SpecifyScript
          , choose (0, 1) >>= (`vectorOf` (elements $ SpecifyRegion <$> regions))
          , manyOf SpecifyVariant
          , manyOf SpecifyExtension
          , oneOrNone SpecifyPrivateUse
          ]

instance Show BCP47 where
  show = T.unpack . toText

instance Read BCP47 where
  readsPrec _ s = case fromText $ T.pack s of
    Left _ -> []
    Right b -> [(b, "")]

-- | Serialize @'BCP47'@ to @'Text'@
--
-- Subtags are serialized in the order described in the BCP 47 specification.
-- Private-use subtags only appear at the end prefixed with an x.
toText :: BCP47 -> Text
toText b =
  T.intercalate "-" $
    mconcat
      [ [languageToText $ language b]
      , mapMaybe fromSubtags . Set.toList $ subtags b
      , if Set.null (privateUse b) then [] else ["x"]
      , map privateUseToText . Set.toList $ privateUse b
      ]
 where
  fromSubtags = \case
    SpecifyLanguageExtension x -> Just $ languageExtensionToText x
    SpecifyScript x -> Just $ scriptToText x
    SpecifyRegion x -> Just $ regionToText x
    SpecifyVariant x -> Just $ variantToText x
    SpecifyExtension x -> Just $ extensionToText x
    SpecifyPrivateUse _ -> Nothing

-- | Look up all language extension subtags
extendedLanguageSubtags :: BCP47 -> Set LanguageExtension
extendedLanguageSubtags = asSet $ \case
  SpecifyLanguageExtension x -> Just x
  _otherwise -> Nothing

-- | Look up the script subtag
script :: BCP47 -> Maybe Script
script = headMay . mapMaybe f . Set.toList . subtags
 where
  f = \case
    SpecifyScript x -> Just x
    _otherwise -> Nothing

-- | Look up the region subtag
region :: BCP47 -> Maybe Country
region = headMay . mapMaybe f . Set.toList . subtags
 where
  f = \case
    SpecifyRegion x -> Just x
    _otherwise -> Nothing

-- | Look up all variant subtags
variants :: BCP47 -> Set Variant
variants = asSet $ \case
  SpecifyVariant x -> Just x
  _otherwise -> Nothing

-- | Look up all extension subtags
extensions :: BCP47 -> Set Extension
extensions = asSet $ \case
  SpecifyExtension x -> Just x
  _otherwise -> Nothing

-- | Look up all private use subtags
privateUse :: BCP47 -> Set PrivateUse
privateUse = asSet $ \case
  SpecifyPrivateUse x -> Just x
  _otherwise -> Nothing

asSet :: Ord a => (Subtags -> Maybe a) -> BCP47 -> Set a
asSet f = Set.fromList . mapMaybe f . Set.toList . subtags

headMay :: [x] -> Maybe x
headMay [] = Nothing
headMay (x : _) = Just x

-- | Convert tag to list of subtags
toSubtags :: BCP47 -> [Subtags]
toSubtags tag = toList $ subtags tag

-- | Produce a list of @(<= priority)@ language tags
--
-- >>> inits <$> fromText (pack "en-GB-t-jp")
-- Right [en,en-GB,en-GB-t-jp]
inits :: BCP47 -> [BCP47]
inits tag =
  map (BCP47 (language tag) . Set.fromList) . List.inits $ toSubtags tag

-- | Construct a simple language tag
mkLanguage :: ISO639_1 -> BCP47
mkLanguage lang = BCP47 lang mempty

-- | Construct a localized tag
mkLocalized :: ISO639_1 -> Country -> BCP47
mkLocalized lang locale = BCP47 lang . Set.singleton $ SpecifyRegion locale

-- | Parse a language tag from text
--
-- >>> fromText $ pack "en"
-- Right en
--
-- >>> fromText $ pack "de-CH"
-- Right de-CH
--
-- >>> fromText $ pack "ru-USR"
-- Left "fromText:1:3:\n  |\n1 | ru-USR\n  |   ^\nunexpected '-'\n"
--
-- >>> fromText $ pack "en-a-ccc-v-qqq-a-bbb"
-- Right en-a-bbb-a-ccc-v-qqq
--
-- >>> fromText $ pack "de-Latn-DE"
-- Right de-Latn-DE
--
-- >>> fromText $ pack "de-Latf-DE"
-- Right de-Latf-DE
--
-- >>> fromText $ pack "de-CH-1996"
-- Right de-CH-1996
--
-- >>> fromText $ pack "de-Deva"
-- Right de-Deva
--
-- >>> fromText $ pack "zh-Hant-CN-x-private1-private2"
-- Right zh-Hant-CN-x-private1-private2
--
-- >>> fromText $ pack "zh-Hant-CN-x-private1"
-- Right zh-Hant-CN-x-private1
--
-- >>> fromText $ pack "zh-Hant-CN"
-- Right zh-Hant-CN
--
-- >>> fromText $ pack "zh-Hant"
-- Right zh-Hant
--
-- >>> fromText $ pack "zh"
-- Right zh
fromText :: Text -> Either Text BCP47
fromText =
  first (pack . errorBundlePretty) . parse (parser <* hidden eof) "fromText"

-- |
--
-- >>> _example $ pack "en;"
-- Right (en,';')
_example :: Text -> Either Text (BCP47, Char)
_example = first (pack . errorBundlePretty) . parse p "example"
 where
  p = (,) <$> parser <*> char ';'

parser :: Parsec Void Text BCP47
parser = BCP47 <$> languageP <*> subtagsP
 where
  subtagsP =
    mconcat
      <$> sequenceA
        [ manyAsSet SpecifyLanguageExtension (try (char '-' *> languageExtensionP))
        , maybe mempty (Set.singleton . SpecifyScript)
            <$> (try (optional $ char '-' *> scriptP) <|> pure Nothing)
        , maybe mempty (Set.singleton . SpecifyRegion)
            <$> (try (optional $ char '-' *> regionP) <|> pure Nothing)
        , manyAsSet SpecifyVariant (try (char '-' *> variantP))
        , manyAsSet SpecifyExtension (try (char '-' *> extensionP))
        , Set.map SpecifyPrivateUse <$> (try (char '-' *> privateUseP) <|> mempty)
        ]

manyAsSet :: (Ord b, MonadPlus m) => (a -> b) -> m a -> m (Set b)
manyAsSet f p = Set.fromList . map f <$> many p

-- | Spanish
es :: BCP47
es = mkLanguage ES

-- | English
en :: BCP47
en = mkLanguage EN

-- | Swahili
sw :: BCP47
sw = mkLanguage SW

-- | British English
enGB :: BCP47
enGB = mkLocalized EN unitedKingdomOfGreatBritainAndNorthernIreland

-- | American English
enUS :: BCP47
enUS = mkLocalized EN unitedStatesOfAmerica
