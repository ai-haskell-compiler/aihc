-- |
--
-- Module      : Network.URI.Template.Internal
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal
  ( Template (..)
  , templateP
  , templatePretty
  , variableP
  , variablePretty
  , module Network.URI.Template.Internal.Expression
  , module Network.URI.Template.Internal.Modifier
  , module Network.URI.Template.Internal.Operator
  , module Network.URI.Template.Internal.TemplatePart
  , module Network.URI.Template.Internal.VarSpec
  ) where

import Prelude

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Network.URI.Template.Internal.Expression
import Network.URI.Template.Internal.Modifier
import Network.URI.Template.Internal.Operator
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.Pretty
import Network.URI.Template.Internal.TemplatePart
import Network.URI.Template.Internal.VarSpec
import Network.URI.Template.VarName
import Network.URI.Template.VarValue

newtype Template = Template
  { unwrap :: [TemplatePart]
  }
  deriving stock (Eq, Show)

templateP :: Parser Template
templateP = Template <$> some templatePartP

templatePretty :: Template -> Doc Ann
templatePretty = mconcat . map templatePartPretty . (.unwrap)

-- | Parse one @name := value@ assignment
variableP :: Parser (Map VarName VarValue)
variableP =
  Map.singleton
    <$> varNameP
    <*> (hspace *> string ":=" *> hspace *> varValueP)

-- | Prettyprint a variable and value, 'fill'ing the name to the given width
variablePretty :: Int -> VarName -> VarValue -> Doc Ann
variablePretty w n v =
  hsep
    [ fill w $ varNamePretty n
    , annotate AnnPunctuation ":="
    , varValuePretty v
    ]
