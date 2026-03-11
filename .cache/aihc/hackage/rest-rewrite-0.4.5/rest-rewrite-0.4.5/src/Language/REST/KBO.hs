{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.REST.KBO (kbo, kboGTE) where

import           Language.REST.OCAlgebra
import           Language.REST.Op
import           Language.REST.RuntimeTerm as RT
import           Language.REST.SMT
import           Language.REST.Internal.Util

import qualified Data.Map as M

termOps :: RuntimeTerm -> [Op]
termOps (App f xs) = f:concatMap termOps xs

arityConstraints :: RuntimeTerm -> SMTExpr Bool
arityConstraints t = toExpr $ go M.empty t where
  go :: M.Map Op Int -> RuntimeTerm -> M.Map Op Int
  go m (App f [])  = M.insert f 1 m
  go m (App f [targ]) = go (M.insert f 1 m) targ
  go m (App f ts)  = foldl go (M.insert f 0 m) ts

  toExpr m = And $ map toConstraint (M.toList m)
  toConstraint (sym, n) = toSMT sym `smtGTE` Const n


-- | @kboGTE t u@ returns the SMT expression describing constraints
-- on the weights of function symbols such that @t@ is greater than @u@
-- in the KBO ordering.
kboGTE :: RuntimeTerm -> RuntimeTerm -> SMTExpr Bool
kboGTE t u = arityConstraints t `smtAnd` arityConstraints u `smtAnd` (size tOps `smtGTE` size uOps)
  where
    (tOps, uOps) = removeEqBy (==) (termOps t) (termOps u)
    size ops     = smtAdd (map toSMT ops)


-- | OCA for a quasi-order extension to the Knuth-Bendix ordering
kbo :: SolverHandle -> OCAlgebra (SMTExpr Bool) RuntimeTerm IO
kbo solver = OCAlgebra
  {  isSat           = checkSat' solver
  ,  refine
  ,  top             = smtTrue
  ,  union
  ,  notStrongerThan
  }
  where
    union  e1 e2          = Or [e1, e2]
    refine e t u          = e `smtAnd` kboGTE t u
    notStrongerThan e1 e2 = checkSat' solver (Implies e2 e1)
