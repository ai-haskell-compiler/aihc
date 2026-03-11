-- |
--
-- Module      : Network.URI.Template.Internal.VarSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.VarSpec
  ( VarSpec (..)
  , varSpecP
  , varSpecPretty
  , expandVarSpec
  ) where

import Prelude

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Network.URI.Template.Internal.Modifier
import Network.URI.Template.Internal.Operator
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.Pretty
import Network.URI.Template.VarName
import Network.URI.Template.VarValue

data VarSpec = VarSpec
  { name :: VarName
  , modifier :: Maybe Modifier
  }
  deriving stock (Eq, Show)

-- |
--
-- @
-- varspec       =  varname [ modifier-level4 ]
-- @
varSpecP :: Parser VarSpec
varSpecP =
  VarSpec
    <$> varNameP
    <*> optional modifierP

varSpecPretty :: VarSpec -> Doc Ann
varSpecPretty v =
  annotate AnnVarName (pretty $ unVarName v.name)
    <> maybe mempty (annotate AnnModifier . modifierPretty) v.modifier

expandVarSpec :: Map VarName VarValue -> OperatorActions -> VarSpec -> [Doc Ann]
expandVarSpec vars oa vs = maybe [] renderValues $ Map.lookup vs.name vars
 where
  renderValues :: VarValue -> [Doc Ann]
  renderValues = \case
    VarNull -> []
    VarValue t
      | Just (Prefix n) <- vs.modifier -> renderValueEsc $ T.take n t
      | otherwise -> renderValueEsc t
    VarList ts
      | null ts -> []
      | Just Explode <- vs.modifier -> concatMap renderValueEsc ts
      | otherwise -> renderValueCsv $ map (pretty . oa.escapeValue) ts
    VarMap kvs
      | null kvs -> []
      | Just Explode <- vs.modifier -> map (uncurry $ renderKeyValue '=') kvs
      | otherwise -> renderValueCsv $ map (uncurry $ renderKeyValue ',') kvs

  renderValue :: Doc Ann -> [Doc Ann]
  renderValue = pure . oa.renderValue vs.name

  renderValueEsc = renderValue . annotate AnnVarValue . pretty . oa.escapeValue

  renderValueCsv :: [Doc Ann] -> [Doc Ann]
  renderValueCsv = renderValue . mconcat . punctuate (annotate AnnPunctuation ",")

  renderKeyValue :: Char -> Text -> Text -> Doc Ann
  renderKeyValue c k v =
    annotate AnnVarName (pretty $ oa.escapeValue k)
      <> annotate AnnPunctuation (pretty c)
      <> annotate AnnVarValue (pretty $ oa.escapeValue v)
