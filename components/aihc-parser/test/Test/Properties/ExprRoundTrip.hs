{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.ExprRoundTrip
  ( prop_exprPrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax
import qualified Data.Text as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.ExprHelpers (genExpr, normalizeExpr, shrinkExpr)
import Test.QuickCheck

prop_exprPrettyRoundTrip :: Expr -> Property
prop_exprPrettyRoundTrip expr =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty expr))
      expected = normalizeExpr expr
   in withMaxSuccess 1000 $
        counterexample (T.unpack source) $
          case parseExpr defaultConfig source of
            ParseErr err ->
              counterexample (errorBundlePretty (Just source) err) False
            ParseOk parsed ->
              let actual = normalizeExpr parsed
               in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

instance Arbitrary Expr where
  arbitrary = resize 5 genExpr
  shrink = shrinkExpr
