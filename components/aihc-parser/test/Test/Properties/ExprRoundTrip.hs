{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.ExprRoundTrip
  ( prop_exprParensMinimal,
    prop_exprPrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Parens (addExprParens)
import Aihc.Parser.Pretty (prettyExpr)
import Aihc.Parser.Syntax
import Data.Data (Data, gmapQl)
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Expr ()
import Test.Properties.Arb.Utils (requiredExtensions)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

exprConfig :: ParserConfig
exprConfig =
  defaultConfig
    { parserExtensions = requiredExtensions
    }

prop_exprPrettyRoundTrip :: Expr -> Property
prop_exprPrettyRoundTrip expr =
  let parenthesized = addExprParens expr
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty parenthesized))
      expected = stripAnnotations parenthesized
   in assertCtorCoverage ["EAnn"] expr $
        counterexample (T.unpack source) $
          case parseExpr exprConfig source of
            ParseErr err ->
              counterexample (MPE.errorBundlePretty err) False
            ParseOk parsed ->
              let actual = stripAnnotations parsed
               in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

prop_exprParensMinimal :: Expr -> Property
prop_exprParensMinimal expr =
  if astSize expr > 80
    then property True
    else
      let source = renderStrict (layoutPretty defaultLayoutOptions (prettyExpr expr))
          expected = stripAnnotations expr
       in assertCtorCoverage ["EAnn"] expr $
            counterexample ("raw source: " <> T.unpack source) $
              if T.length source > 120
                then property True
                else case parseExpr exprConfig source of
                  ParseErr _ ->
                    property True
                  ParseOk parsed ->
                    let parsedExpr = stripAnnotations parsed
                        actual = stripAnnotations (addExprParens expr)
                        validRawExpr = parsedExpr == expected
                        failure =
                          unlines
                            [ "raw source: " <> T.unpack source,
                              "parsed: " <> show parsedExpr,
                              "expected: " <> show expected,
                              "actual: " <> show actual
                            ]
                     in cover 1 validRawExpr "valid raw expression" $
                          if validRawExpr
                            then counterexample failure (actual == expected)
                            else property True

astSize :: (Data a) => a -> Int
astSize x = 1 + gmapQl (+) 0 astSize x
