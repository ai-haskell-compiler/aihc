{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- We need redundant constraints in @widen@ to enforce invariants
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Internal module of NonEmptyText, allowing breaking the abstraction.
--
--   Prefer to use "Data.StringVariants.NonEmptyText" instead.

module Data.StringVariants.NonEmptyText.Internal where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.ByteString
import Data.Coerce
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.MonoTraversable
import Data.Proxy
import Data.Sequences
import Data.String.Conversions (ConvertibleStrings (..), cs)
import Data.StringVariants.Util (textHasNoMeaningfulContent, textIsWhitespace)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, TypeError, natVal, type (<=))
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift (..), TyLit (..), Type (..))
import Test.QuickCheck
import Prelude

-- | Non Empty Text, requires the input is between 1 and @n@ chars and not just whitespace.
newtype NonEmptyText (n :: Nat) = NonEmptyText Text
  deriving stock (Data, Generic, Show, Read, Lift)
  deriving newtype (Eq, Ord, ToJSON, MonoFoldable, Hashable, NFData)

type instance Element (NonEmptyText _n) = Char

instance (KnownNat n, 1 <= n) => FromJSON (NonEmptyText n) where
  parseJSON = withText "NonEmptyText" $ \t -> do
    performInboundValidations t
    case mkNonEmptyText t of
      Nothing -> fail $ "Data/StringVariants/NonEmptyText.hs: invalid NonEmptyText " ++ show (natVal (Proxy @n))  ++ ": " ++ T.unpack t
      Just nonEmptyText -> pure nonEmptyText
    where
      -- These validations are performed at the edge of the system rather than in
      -- mkNonEmptyText because there may be cases where legacy NonEmptyText is
      -- stored in the database that fits one of these situations.
      --
      -- However, we don't want new stuff to send junk data, so we use this FromJSON
      -- instance to validate at the edge.
      performInboundValidations t = do
        when (textHasNoMeaningfulContent t) $
          fail $
            "Data/StringVariants/NonEmptyText.hs: NonEmptyText has no meaningful content in the field: " ++ T.unpack t
        when (T.any (== '\NUL') t) $
          fail $
            "Data/StringVariants/NonEmptyText.hs: NonEmptyText has a \\NUL byte in its contents: " ++ T.unpack t

-- Could add more instances of this (for lazy text, bytestrings, etc) if we think we need them.
instance ConvertibleStrings (NonEmptyText n) Text where
  convertString (NonEmptyText t) = t

instance ConvertibleStrings (NonEmptyText n) String where
  convertString (NonEmptyText t) = cs t

instance ConvertibleStrings (NonEmptyText n) ByteString where
  convertString (NonEmptyText t) = cs t

instance (KnownNat n, 1 <= n) => Arbitrary (NonEmptyText n) where
  arbitrary =
    NonEmptyText @n <$> do
      size <- chooseInt (1, fromIntegral (natVal (Proxy :: Proxy n)) - 1)
      -- Mostly alphanumeric characters, all human readable
      str <- replicateM size $ elements ['0' .. 'z']
      pure $ T.pack str

instance
  TypeError
        ( 'Text "An instance of 'Semigroup (NonEmptyText n)' would violate the "
    ' :<>: 'Text "length guarantees."
    ' :$$: 'Text "Please use '(<>|)' or 'concatWithSpace' to combine the values."
        )
  => Semigroup (NonEmptyText n) where
  (<>) = error "unreachable"

mkNonEmptyText :: forall n. (KnownNat n, 1 <= n) => Text -> Maybe (NonEmptyText n)
mkNonEmptyText t
  | T.compareLength stripped (fromIntegral $ natVal (Proxy @n)) == GT = Nothing
  | textIsWhitespace stripped = Nothing
  | otherwise = Just (NonEmptyText stripped)
  where
    stripped = T.filter (/= '\NUL') $ T.strip t

mkNonEmptyTextWithTruncate :: forall n. (KnownNat n, 1 <= n) => Text -> Maybe (NonEmptyText n)
mkNonEmptyTextWithTruncate t
  | textIsWhitespace stripped = Nothing
  | otherwise = Just (NonEmptyText $ T.stripEnd $ T.take (fromIntegral $ natVal (Proxy @n)) stripped)
  where
    stripped = T.filter (/= '\NUL') $ T.stripStart t

-- | Make a NonEmptyText when you can manually verify the length
unsafeMkNonEmptyText :: forall n. (KnownNat n, 1 <= n) => Text -> NonEmptyText n
unsafeMkNonEmptyText = NonEmptyText

-- | Converts a 'NonEmptyText' to a wider NonEmptyText
widen :: (1 <= n, n <= m) => NonEmptyText n -> NonEmptyText m
widen = coerce

compileNonEmptyTextKnownLength :: QuasiQuoter
compileNonEmptyTextKnownLength =
  QuasiQuoter
    { quoteExp = \s -> do
        let txt = T.pack s
        if textHasNoMeaningfulContent txt
          then fail "Invalid NonEmptyText. Must have at least one non-whitespace, non-control character."
          else [|NonEmptyText $(lift txt) :: NonEmptyText $(pure $ LitT $ NumTyLit $ fromIntegral $ T.length txt)|]
    , quotePat = error "Known-length NonEmptyText is not currently supported as a pattern"
    , quoteDec = error "NonEmptyText is not supported at top-level"
    , quoteType = error "NonEmptyText is not supported as a type"
    }
