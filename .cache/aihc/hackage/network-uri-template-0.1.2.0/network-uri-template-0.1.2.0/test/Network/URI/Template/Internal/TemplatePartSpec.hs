-- |
--
-- Module      : Network.URI.Template.Internal.TemplatePartSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.TemplatePartSpec
  ( spec
  ) where

import Prelude

import Network.URI.Template.Internal.Expression
import Network.URI.Template.Internal.Operator
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.TemplatePart
import Network.URI.Template.Internal.VarSpec
import Network.URI.Template.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "templatePartP" $ do
    it "parses a URI with query variable"
      $ assertParse
        (some templatePartP)
        "foo/{?var}&bat"
        [ Lit "foo/"
        , Exp
            $ Expression
              { operator = Just Query
              , variableList = [VarSpec {name = "var", modifier = Nothing}]
              }
        , Lit "&bat"
        ]

    it "parses a template with literal at end of input" $ do
      assertParse
        (some templatePartP <* eof)
        "{&who}X"
        [ Exp
            $ Expression
              { operator = Just QueryContinuation
              , variableList = [VarSpec {name = "who", modifier = Nothing}]
              }
        , Lit "X"
        ]

    it "parses a template at end of line" $ do
      assertParse
        (sepEndBy (some templatePartP) eol <* eof)
        "{&who}X\n"
        [
          [ Exp
              $ Expression
                { operator = Just QueryContinuation
                , variableList = [VarSpec {name = "who", modifier = Nothing}]
                }
          , Lit "X"
          ]
        ]

    it "parses a template within RFC examples" $ do
      let
        p =
          (,)
            <$> (hspace1 *> some templatePartP)
            <*> (hspace1 *> manyTill anySingle eof)

        input = "  {&who}  &who=fred"
        expected =
          (
            [ Exp
                $ Expression
                  { operator = Just QueryContinuation
                  , variableList = [VarSpec {name = "who", modifier = Nothing}]
                  }
            ]
          , "&who=fred"
          )

      assertParse p input expected
