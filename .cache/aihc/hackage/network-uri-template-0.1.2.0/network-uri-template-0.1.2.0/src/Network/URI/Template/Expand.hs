-- |
--
-- Module      : Network.URI.Template.Expand
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Expand
  ( expandTemplate
  , expandTemplateDoc
  ) where

import Prelude

import Data.Map.Strict (Map)
import Data.Text (Text, pack)
import Network.URI.Template.Internal
import Network.URI.Template.Internal.Pretty (Ann, Doc, pretty, renderPlain)
import Network.URI.Template.VarName
import Network.URI.Template.VarValue

expandTemplate :: Map VarName VarValue -> Template -> Text
expandTemplate env = pack . renderPlain . expandTemplateDoc env

expandTemplateDoc :: Map VarName VarValue -> Template -> Doc Ann
expandTemplateDoc env = foldMap (expandTemplatePart env) . (.unwrap)

expandTemplatePart :: Map VarName VarValue -> TemplatePart -> Doc Ann
expandTemplatePart env = \case
  Lit t -> pretty t
  Exp e -> expandExpression env e
