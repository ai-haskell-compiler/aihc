-- |
--
-- Module      : Network.URI.Template.Internal.Pretty
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.Pretty
  ( Ann (..)
  , renderPlain

    -- * Re-exports
  , module Prettyprinter
  ) where

import Prelude

import Prettyprinter
import Prettyprinter.Render.String

data Ann
  = AnnPunctuation
  | AnnOperator
  | AnnIntercalate
  | AnnVarName
  | AnnVarValue
  | AnnModifier
  | AnnString

renderPlain :: Doc ann -> String
renderPlain = renderString . layoutPretty defaultLayoutOptions
