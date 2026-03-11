-- |
--
-- Module      : Network.URI.Template
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template
  ( Template
  , TemplateError (..)
  , templateErrorPretty
  , processTemplate

    -- * Template variables
  , module Network.URI.Template.VarName
  , module Network.URI.Template.VarValue
  ) where

import Prelude

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Network.URI.Template.Expand
import Network.URI.Template.Internal
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Parse
import Network.URI.Template.VarName
import Network.URI.Template.VarValue

newtype TemplateError = TemplateParseError ParseError

templateErrorPretty :: TemplateError -> String
templateErrorPretty = \case
  TemplateParseError pe -> errorBundlePretty pe

processTemplate :: Map VarName VarValue -> Text -> Either TemplateError Text
processTemplate env t = do
  template <- first TemplateParseError $ parseTemplate t
  pure $ expandTemplate env template
