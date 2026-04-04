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
import qualified Text.Megaparsec.Error as MPE

exprConfig :: ParserConfig
exprConfig =
  defaultConfig
    { parserExtensions = [UnboxedTuples, UnboxedSums, TemplateHaskell]
    }

prop_exprPrettyRoundTrip :: Expr -> Property
prop_exprPrettyRoundTrip expr =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty expr))
      expected = normalizeExpr expr
   in withMaxSuccess 1000 $
        counterexample (T.unpack source) $
          case parseExpr exprConfig source of
            ParseErr err ->
              counterexample (MPE.errorBundlePretty err) False
            ParseOk parsed ->
              let actual = normalizeExpr parsed
               in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

instance Arbitrary Expr where
  arbitrary = resize 5 genExpr
  shrink = shrinkExpr
