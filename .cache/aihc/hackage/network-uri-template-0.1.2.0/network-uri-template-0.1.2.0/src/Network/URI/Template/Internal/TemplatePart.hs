-- |
--
-- Module      : Network.URI.Template.Internal.TemplatePart
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.TemplatePart
  ( TemplatePart (..)
  , templatePartP
  , templatePartPretty
  ) where

import Prelude

import Data.Text (Text, pack)
import Network.URI.Template.Internal.Expression
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.Pretty

data TemplatePart
  = Lit Text
  | Exp Expression
  deriving stock (Eq, Show)

templatePartP :: Parser TemplatePart
templatePartP =
  choice
    [ Exp <$> expressionP <?> "template expression"
    , Lit . pack <$> some litChar <?> "template literal"
    ]
 where
  litChar :: Parser Char
  litChar = noneOf ['\r', '\n', '\t', ' ', '{']

templatePartPretty :: TemplatePart -> Doc Ann
templatePartPretty = \case
  Lit t -> pretty t
  Exp e -> expressionPretty e
