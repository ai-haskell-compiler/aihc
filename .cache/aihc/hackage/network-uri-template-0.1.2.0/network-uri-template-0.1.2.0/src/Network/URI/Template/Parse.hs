-- |
--
-- Module      : Network.URI.Template.Parse
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Parse
  ( parseTemplate

    -- * Errors
  , ParseError
  , errorBundlePretty
  ) where

import Prelude

import Data.Text (Text)
import Network.URI.Template.Internal
import Network.URI.Template.Internal.Parse

parseTemplate :: Text -> Either ParseError Template
parseTemplate = parse templateP
