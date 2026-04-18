{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.ExprRoundTrip
  ( prop_exprPrettyRoundTrip,
    test_exprPrettyRoundTrip_qualifiedUnicodeOperatorNameQuote,
  )
where

import Aihc.Parser
import Aihc.Parser.Parens (addExprParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Expr ()
import Test.Properties.Coverage (assertCtorCoverage)
import Test.Properties.ExprHelpers (normalizeExpr, span0)
import Test.QuickCheck
import Test.Tasty.HUnit (Assertion, assertFailure)
import Text.Megaparsec.Error qualified as MPE

exprConfig :: ParserConfig
exprConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell, MagicHash, OverloadedLabels, MultiWayIf, RecursiveDo, TypeApplications, TupleSections, ImplicitParams, ExplicitNamespaces, TypeAbstractions, RequiredTypeArguments, LambdaCase]
    }

prop_exprPrettyRoundTrip :: Expr -> Property
prop_exprPrettyRoundTrip expr =
  let parenthesized = addExprParens expr
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty parenthesized))
      expected = normalizeExpr parenthesized
   in assertCtorCoverage ["EAnn"] expr $
        counterexample (T.unpack source) $
          case parseExpr exprConfig source of
            ParseErr err ->
              counterexample (MPE.errorBundlePretty err) False
            ParseOk parsed ->
              let actual = normalizeExpr parsed
               in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

test_exprPrettyRoundTrip_qualifiedUnicodeOperatorNameQuote :: Assertion
test_exprPrettyRoundTrip_qualifiedUnicodeOperatorNameQuote =
  let expr = EAnn (mkAnnotation span0) (ETHNameQuote (mkName (Just "H3xVBC.NB.Y") NameVarSym "‼."))
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty expr))
      expected = normalizeExpr (addExprParens expr)
   in case parseExpr exprConfig source of
        ParseErr err -> assertFailure ("expected parse success for " <> T.unpack source <> "\n" <> MPE.errorBundlePretty err)
        ParseOk parsed ->
          let actual = normalizeExpr parsed
           in if actual == expected
                then pure ()
                else assertFailure ("expected: " <> show expected <> "\nactual: " <> show actual)
