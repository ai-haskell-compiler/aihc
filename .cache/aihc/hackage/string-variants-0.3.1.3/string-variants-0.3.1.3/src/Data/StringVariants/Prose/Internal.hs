{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- GHC considers the constraints for the prose symbol redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Internal module of Prose, allowing breaking the abstraction.
--
--   Prefer to use "Data.StringVariants.Prose" instead.
module Data.StringVariants.Prose.Internal where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey, withText)
import Data.Aeson.Types (FromJSON (..))
import Data.String.Conversions (ConvertibleStrings (..), cs)
import Data.StringVariants.NonEmptyText.Internal (NonEmptyText (..))
import Data.StringVariants.Util (SymbolWithNoSpaceAround)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Prelude

-- | Whitespace-trimmed, non-empty text, for use with API endpoints.
-- The rationale is that there are many situations where if a client sends
-- text that is empty or all whitespace, there's probably a client error.
-- Not suitable for database fields, as there is no character limit (see
-- 'ProsePersistFieldMsg').
newtype Prose = Prose Text
  deriving stock (Eq, Lift, Ord, Show)
  deriving newtype (Semigroup, ToJSON, ToJSONKey)

instance ConvertibleStrings Prose Text where
  convertString (Prose t) = t

instance ConvertibleStrings Prose LT.Text where
  convertString (Prose t) = cs t

instance FromJSON Prose where
  parseJSON = withText "Prose" $ \t -> case mkProse t of
    Nothing -> fail $ "Model/CustomTypes/StringVariants.hs: invalid Prose: " ++ T.unpack t
    Just t' -> pure t'

mkProse :: Text -> Maybe Prose
mkProse t = case T.strip t of
  "" -> Nothing
  t' -> Just (Prose t')

proseFromNonEmptyText :: NonEmptyText n -> Prose
proseFromNonEmptyText (NonEmptyText t) = Prose (T.strip t)

compileProse :: QuasiQuoter
compileProse =
  QuasiQuoter
    { quoteDec = error "Prose is not supported at top-level"
    , quoteType = error "Prose is not supported as a type"
    , quotePat = error "Prose is not a pattern; use `proseToText` instead"
    , ..
    }
  where
    quoteExp s = case mkProse (T.pack s) of
      Nothing -> fail (msg s)
      Just s' -> [|$(lift s')|]

    msg s = "Invalid Prose: " <> s <> ". Make sure you aren't wrapping the text in quotes."

type IsProse s =
  ( KnownSymbol s
  , SymbolWithNoSpaceAround s
  )

literalProse :: forall (s :: Symbol). IsProse s => Prose
literalProse = Prose (T.pack (symbolVal (Proxy @s)))

proseToText :: Prose -> Text
proseToText (Prose txt) = txt
