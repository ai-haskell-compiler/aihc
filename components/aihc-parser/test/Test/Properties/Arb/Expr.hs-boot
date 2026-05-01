{-# LANGUAGE Haskell2010 #-}

-- | Boot file for Test.Properties.Arb.Expr
-- This breaks the circular dependency between Expr.hs and Pattern.hs.
-- Pattern.hs needs genExpr/shrinkExpr for view patterns and splices,
-- while Expr.hs needs genPattern from Pattern.hs.
module Test.Properties.Arb.Expr where

import Aihc.Parser.Syntax (Expr, GuardQualifier, Rhs)
import Test.QuickCheck (Gen)

genExpr :: Gen Expr
genViewPatternExpr :: Gen Expr
genRhs :: Gen (Rhs Expr)
shrinkExpr :: Expr -> [Expr]
shrinkGuardQualifier :: GuardQualifier -> [GuardQualifier]
