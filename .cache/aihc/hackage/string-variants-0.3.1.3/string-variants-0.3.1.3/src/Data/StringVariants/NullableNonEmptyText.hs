{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.StringVariants.NullableNonEmptyText
  ( -- Safe to construct, so we can export the contents
    NullableNonEmptyText (..),

    -- * Constructing
    mkNonEmptyTextWithTruncate,
    compileNullableNonEmptyText,
    IsNullableNonEmptyText,
    literalNullableNonEmptyText,
    mkNullableNonEmptyText,
    parseNullableNonEmptyText,
    nullNonEmptyText,

    -- * Conversion
    maybeTextToTruncateNullableNonEmptyText,
    nonEmptyTextToNullable,
    maybeNonEmptyTextToNullable,
    nullableNonEmptyTextToMaybeText,
    nullableNonEmptyTextToMaybeNonEmptyText,
    fromMaybeNullableText,

    -- * Functions
    isNullNonEmptyText,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Aeson.Types qualified as J
import Data.Data (Data, Proxy (..))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.MonoTraversable (MonoFoldable)
import Data.StringVariants.NonEmptyText
import Data.StringVariants.NonEmptyText.Internal (NonEmptyText (..))
import Data.StringVariants.Util
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude

-- | Newtype wrapper around Maybe NonEmptyText that converts empty string to 'Nothing'.
--
--   @'NullableNonEmptyText' n@ is used in API types to represent optional text fields when you do not want an empty string to fail to parse.
--   Like 'NonEmptyText', the payload 'Text' is guaranteed to be non-empty, within the character limit, and stripped of whitespace.
--   Unlike 'NonEmptyText', it will successfully parse empty strings as 'nullNonEmptyText'.
--
--   Since Aeson version 2.2, fields of this type maybe be missing, @null@, or empty without failing to parse.
--   Avoid using @Maybe (NullableNonEmptyText n)@ in API types, since it creates unnecessary edge cases that complicate the code.
--
--   __NB:__ When using a version of Aeson prior to 2.2, you /must/ use @Maybe (NullableNonEmptyText n)@ if you want to allow missing or null fields to parse.
--
--   @
--   data Person = Person
--     { name :: 'NonEmptyText' 50
--     , catchphrase :: 'NullableNonEmptyText' 500
--     }
--   @
--
--   With this type definition, these four JSON objects below are valid and parse as @Person "Daniel" nullNonEmptyText@.
--
--   > {"name": "Daniel"}
--   > {"name": "Daniel", catchphrase: null}
--   > {"name": "Daniel", catchphrase: ""}
--   > {"name": "Daniel", catchphrase: " "}
--
--   These two JSON objects parses as @Person "Daniel" (mkNullableNonEmptyText "Yabba-Dabba Do!")@
--
--   > {"name": "Daniel", catchphrase: "Yabba-Dabba Do!"}
--   > {"name": "Daniel", catchphrase: "    Yabba-Dabba Do!   "}
--
--   Use 'nullableNonEmptyTextToMaybeNonEmptyText' to extract @Maybe (NonEmptyText n)@ from @NullableNonEmptyText n@.
newtype NullableNonEmptyText n = NullableNonEmptyText (Maybe (NonEmptyText n))
  deriving stock (Data, Generic, Show, Read, Lift)
  deriving newtype (Eq, Ord, Hashable, NFData)

mkNullableNonEmptyText :: forall n. (KnownNat n, 1 <= n) => Text -> Maybe (NullableNonEmptyText n)
mkNullableNonEmptyText t
  | T.compareLength stripped (fromIntegral $ natVal (Proxy @n)) == GT = Nothing -- we can't store text that is too long
  | otherwise = Just $ NullableNonEmptyText $ mkNonEmptyText t
  where
    stripped = T.filter (/= '\NUL') $ T.strip t

nullNonEmptyText :: NullableNonEmptyText n
nullNonEmptyText = NullableNonEmptyText Nothing

isNullNonEmptyText :: NullableNonEmptyText n -> Bool
isNullNonEmptyText = (== nullNonEmptyText)

instance ToJSON (NullableNonEmptyText n) where
  toJSON (NullableNonEmptyText t) = toJSON t

  toEncoding (NullableNonEmptyText t) = toEncoding t

#if MIN_VERSION_aeson(2,2,0)
  omitField (NullableNonEmptyText Nothing) = True
  omitField _ = False
#endif

instance (KnownNat n, 1 <= n) => FromJSON (NullableNonEmptyText n) where
  parseJSON = \case
    J.String t -> case mkNullableNonEmptyText t of
      Just txt -> pure txt
      Nothing -> fail $ "Data/StringVariants/NullableNonEmptyText.hs: When trying to parse a NullableNonEmptyText, expected a String of length <= " ++ show (natVal (Proxy @n)) ++ ", but received: " ++ T.unpack t
    J.Null -> pure $ NullableNonEmptyText Nothing
    x -> fail $ "Data/StringVariants/NullableNonEmptyText.hs: When trying to parse a NullableNonEmptyText, expected a String or Null, but received: " ++ show x

#if MIN_VERSION_aeson(2,2,0)
  omittedField = Just (NullableNonEmptyText Nothing)
#endif

parseNullableNonEmptyText :: (KnownNat n, 1 <= n) => Text -> J.Object -> J.Parser (NullableNonEmptyText n)
parseNullableNonEmptyText fieldName obj = obj .:? J.fromText fieldName .!= nullNonEmptyText

fromMaybeNullableText :: Maybe (NullableNonEmptyText n) -> NullableNonEmptyText n
fromMaybeNullableText = fromMaybe nullNonEmptyText

nonEmptyTextToNullable :: NonEmptyText n -> NullableNonEmptyText n
nonEmptyTextToNullable = NullableNonEmptyText . Just

maybeNonEmptyTextToNullable :: Maybe (NonEmptyText n) -> NullableNonEmptyText n
maybeNonEmptyTextToNullable Nothing = nullNonEmptyText
maybeNonEmptyTextToNullable jt = NullableNonEmptyText jt

maybeTextToTruncateNullableNonEmptyText :: forall n. (KnownNat n, 1 <= n) => Maybe Text -> NullableNonEmptyText n
maybeTextToTruncateNullableNonEmptyText mText = NullableNonEmptyText $ mText >>= mkNonEmptyText . T.take (fromInteger (natVal (Proxy @n)))

nullableNonEmptyTextToMaybeNonEmptyText :: NullableNonEmptyText n -> Maybe (NonEmptyText n)
nullableNonEmptyTextToMaybeNonEmptyText (NullableNonEmptyText t) = t

nullableNonEmptyTextToMaybeText :: NullableNonEmptyText n -> Maybe Text
nullableNonEmptyTextToMaybeText (NullableNonEmptyText t) = nonEmptyTextToText <$> t

compileNullableNonEmptyText :: Integer -> QuasiQuoter
compileNullableNonEmptyText n =
  QuasiQuoter
    { quoteExp = compileNullableNonEmptyText'
    , quotePat = error "NullableNonEmptyText is not a pattern; use `nullableNonEmptyTextToMaybeText` instead"
    , quoteDec = error "NullableNonEmptyText is not supported at top-level"
    , quoteType = error "NullableNonEmptyText is not supported as a type"
    }
  where
    compileNullableNonEmptyText' :: String -> Q Exp
    compileNullableNonEmptyText' s = usePositiveNat n errorMessage $ \(_ :: proxy n) ->
      case mkNullableNonEmptyText @n (T.pack s) of
        Just txt -> [|$(lift txt)|]
        Nothing -> errorMessage
      where
        errorMessage = fail $ "Invalid NullableNonEmptyText. Needs to be < " ++ show (n + 1) ++ " characters, and not entirely whitespace: " ++ s

type IsNullableNonEmptyText n s =
  ( KnownSymbol s
  , KnownNat n
  , SymbolNonEmpty s
  , SymbolWithNoSpaceAround s
  , SymbolNoLongerThan s n
  )

-- | This requires the text to be non-empty. For the empty text just use the constructor `NullableNonEmptyText Nothing`
literalNullableNonEmptyText :: forall (s :: Symbol) (n :: Nat). IsNullableNonEmptyText n s => NullableNonEmptyText n
literalNullableNonEmptyText = NullableNonEmptyText (Just (literalNonEmptyText @s @n))
