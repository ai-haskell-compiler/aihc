{-# LANGUAGE Haskell2010 #-}

-- | Boot file for Test.Properties.Arb.Expr
-- This breaks the circular dependency between Expr.hs and Pattern.hs.
-- Pattern.hs needs genExpr/shrinkExpr for view patterns and splices,
-- while Expr.hs needs genPattern from Pattern.hs.
module Test.Properties.Arb.Expr where

import Aihc.Parser.Syntax (Expr)
import Test.QuickCheck (Gen)

genExpr :: Gen Expr
shrinkExpr :: Expr -> [Expr]
