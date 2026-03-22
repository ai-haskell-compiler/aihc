-- | Shared helper functions for expression generation and normalization
-- used by both ExprRoundTrip and ModuleRoundTrip tests.
module Test.Properties.ExprHelpers
  ( genExpr,
    mkIntExpr,
    shrinkExpr,
    normalizeExpr,
    span0,
  )
where

import qualified Data.Text as T
import Parser.Ast
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

-- | Canonical empty source span for normalization.
span0 :: SourceSpan
span0 = noSourceSpan

-- | Generate a random expression up to the given depth.
genExpr :: Int -> Gen Expr
genExpr depth
  | depth <= 0 = oneof [EVar span0 <$> genIdent, mkIntExpr <$> chooseInteger (0, 999)]
  | otherwise =
      frequency
        [ (3, EVar span0 <$> genIdent),
          (3, mkIntExpr <$> chooseInteger (0, 999)),
          (4, EApp span0 <$> genExpr (depth - 1) <*> genExpr (depth - 1))
        ]

-- | Create an integer expression with canonical representation.
mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt span0 value (T.pack (show value))

-- | Shrink an expression for QuickCheck counterexample minimization.
shrinkExpr :: Expr -> [Expr]
shrinkExpr expr =
  case expr of
    EVar _ name -> [EVar span0 shrunk | shrunk <- shrinkIdent name]
    EInt _ value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EApp _ fn arg ->
      [fn, arg]
        <> [EApp span0 fn' arg | fn' <- shrinkExpr fn]
        <> [EApp span0 fn arg' | arg' <- shrinkExpr arg]
    EParen _ inner -> inner : [EParen span0 inner' | inner' <- shrinkExpr inner]
    _ -> []

-- | Normalize an expression to canonical form for comparison.
-- Removes source spans and simplifies parentheses.
normalizeExpr :: Expr -> Expr
normalizeExpr expr =
  case expr of
    EVar _ name -> EVar span0 name
    EInt _ value _ -> mkIntExpr value
    EApp _ fn arg -> EApp span0 (normalizeExpr fn) (normalizeExpr arg)
    EParen _ inner -> normalizeExpr inner
    _ -> expr
