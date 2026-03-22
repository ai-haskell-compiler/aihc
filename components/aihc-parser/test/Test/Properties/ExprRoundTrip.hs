{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.ExprRoundTrip
  ( prop_exprPrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Ast
import Aihc.Parser.Pretty (prettyExpr)
import qualified Data.Text as T
import Test.Properties.ExprHelpers (genExpr, normalizeExpr, shrinkExpr)
import Test.QuickCheck

prop_exprPrettyRoundTrip :: Expr -> Property
prop_exprPrettyRoundTrip expr =
  let source = prettyExpr expr
      expected = normalizeExpr expr
   in checkCoverage $
        exprCoverage expr $
          counterexample (T.unpack source) $
            case parseExpr defaultConfig source of
              ParseErr err ->
                counterexample (errorBundlePretty err) False
              ParseOk parsed ->
                let actual = normalizeExpr parsed
                 in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

exprCoverage :: Expr -> Property -> Property
exprCoverage expr =
  cover 20 (hasVarExpr expr) "contains variable"
    . cover 20 (hasIntExpr expr) "contains integer"
    . cover 20 (hasAppExpr expr) "contains application"

instance Arbitrary Expr where
  arbitrary = sized (genExpr . min 5)
  shrink = shrinkExpr

hasVarExpr :: Expr -> Bool
hasVarExpr expr =
  case expr of
    EVar _ _ -> True
    EApp _ fn arg -> hasVarExpr fn || hasVarExpr arg
    EParen _ inner -> hasVarExpr inner
    _ -> False

hasIntExpr :: Expr -> Bool
hasIntExpr expr =
  case expr of
    EInt {} -> True
    EApp _ fn arg -> hasIntExpr fn || hasIntExpr arg
    EParen _ inner -> hasIntExpr inner
    _ -> False

hasAppExpr :: Expr -> Bool
hasAppExpr expr =
  case expr of
    EApp {} -> True
    EParen _ inner -> hasAppExpr inner
    _ -> False
