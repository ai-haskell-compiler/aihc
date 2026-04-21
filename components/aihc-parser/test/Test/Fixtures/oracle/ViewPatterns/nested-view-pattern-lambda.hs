{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module NestedViewPatternLambda where

import Language.Haskell.TH

toExp :: Expr -> TH.Exp
toExp d (Expr.HsLam _ _ (Expr.MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (unLoc -> map unLoc -> ps) (Expr.GRHSs _ (NE.toList -> [unLoc -> Expr.GRHS _ _ (unLoc -> e)]) _)])))) = TH.LamE (fmap (toPat d) ps) (toExp d e)
