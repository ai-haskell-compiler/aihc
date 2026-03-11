-- |
--
-- Module      : Network.URI.Template.Internal.ExpressionSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.ExpressionSpec
  ( spec
  ) where

import Prelude

import Network.URI.Template.Internal.Expression
import Network.URI.Template.Internal.Modifier
import Network.URI.Template.Internal.Operator
import Network.URI.Template.Internal.VarSpec
import Network.URI.Template.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "expressionP" $ do
    it "parses a query variable"
      $ assertParse expressionP "{?var}"
      $ Expression
        { operator = Just Query
        , variableList = [VarSpec {name = "var", modifier = Nothing}]
        }

    it "parses multiple variables"
      $ assertParse expressionP "{?var,var*}"
      $ Expression
        { operator = Just Query
        , variableList =
            [ VarSpec {name = "var", modifier = Nothing}
            , VarSpec {name = "var", modifier = Just Explode}
            ]
        }
