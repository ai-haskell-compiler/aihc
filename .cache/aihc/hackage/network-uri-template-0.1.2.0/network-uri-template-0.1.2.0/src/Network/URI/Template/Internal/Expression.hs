-- |
--
-- Module      : Network.URI.Template.Internal.Expression
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.Expression
  ( Expression (..)
  , expressionP
  , expressionPretty
  , expandExpression
  ) where

import Prelude

import Data.Map.Strict (Map)
import Network.URI.Template.Internal.Operator
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.Pretty
import Network.URI.Template.Internal.VarSpec
import Network.URI.Template.VarName
import Network.URI.Template.VarValue

data Expression = Expression
  { operator :: Maybe Operator
  , variableList :: [VarSpec]
  }
  deriving stock (Eq, Show)

-- |
--
-- @
-- expression    =  "{" [ operator ] variable-list "}"
-- variable-list =  varspec *( "," varspec )
-- @
expressionP :: Parser Expression
expressionP =
  Expression
    <$> (char '{' *> optional operatorP)
    <*> (sepBy1 varSpecP (char ',') <* char '}')

expressionPretty :: Expression -> Doc Ann
expressionPretty e = do
  annotate AnnPunctuation "{"
    <> maybe mempty (annotate AnnOperator . operatorPretty) e.operator
    <> hcat (punctuate "," $ map varSpecPretty e.variableList)
    <> annotate AnnPunctuation "}"

expandExpression :: Map VarName VarValue -> Expression -> Doc Ann
expandExpression env e =
  prefix
    <> mconcat (punctuate (annotate AnnIntercalate $ pretty oa.listIntercalate) vars)
 where
  oa = maybe nullOperatorActions operatorActions e.operator

  prefix
    | null vars = ""
    | otherwise = annotate AnnOperator $ pretty oa.listPrefix

  vars = mconcat $ map (expandVarSpec env oa) e.variableList
