-- |
--
-- Module      : Network.URI.Template.Internal.Modifier
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.Modifier
  ( Modifier (..)
  , modifierP
  , modifierPretty
  ) where

import Prelude

import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.Pretty

data Modifier
  = Prefix Int
  | Explode
  deriving stock (Eq, Show)

-- |
--
-- @
-- modifier-level4 =  prefix / explode
--
-- prefix     =  ":" max-length
-- max-length =  %x31-39 0*3DIGIT   ; positive integer < 10000
--
-- explode = *
-- @
modifierP :: Parser Modifier
modifierP = prefixP <|> explodeP

modifierPretty :: Modifier -> Doc ann
modifierPretty = \case
  Prefix n -> ":" <> pretty n
  Explode -> "*"

prefixP :: Parser Modifier
prefixP = char ':' *> (Prefix . read <$> some digitChar) <?> "prefix modifier"

explodeP :: Parser Modifier
explodeP = Explode <$ char '*' <?> "explode modifier"
