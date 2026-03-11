

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Language.REST.LPO (lpo, lpoStrict) where

import Prelude hiding (EQ, GT, lex)

import Control.Monad.Identity
import Data.Hashable

import           Language.REST.Op
import           Language.REST.Internal.OpOrdering as OpOrdering
import           Language.REST.WQOConstraints as OC
import           Language.REST.Types
import           Language.REST.RuntimeTerm

lex
  :: (Eq a, Ord b, Hashable b)
  => WQOConstraints impl m
  -> Bool
  -> impl b
  -> (WQOConstraints impl m -> Relation -> impl b -> a -> a -> impl b)
  -> [a]
  -> [a]
  -> impl b
lex oc strict cs f (t:ts) (u:us) | t == u = lex oc strict cs f ts us
lex oc strict cs f (t:ts) (u:us) = union oc case1 case2
  where
    -- t > u
    case1 = f oc GT cs t u
    -- t = u
    case2 =
      let
        cs' = f oc EQ cs t u
      in
        lex oc strict cs' f ts us
lex oc _      _  _ []    (_:_) = unsatisfiable oc
lex _  _      cs _ (_:_) []    = cs
lex oc strict cs _ []    []    = if strict then unsatisfiable oc else cs

lpo' :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) =>
  Bool -> WQOConstraints oc m -> Relation -> oc Op -> RuntimeTerm -> RuntimeTerm -> oc Op
-- lpo' False oc EQ cs t u = intersect oc (lpo' False oc GTE cs t u) (lpo' False oc GTE cs u t)
lpo' False oc EQ _cs (App _f ts) (App _g us) | length ts /= length us = unsatisfiable oc
lpo' False oc EQ cs (App f ts) (App g us) =
  let
    cs'  = intersect oc cs (singleton oc $ f =. g)
    subs = zipWith (lpo' False oc EQ cs') ts us
  in
    intersectAll oc (cs' : subs)

lpo' True  oc EQ cs t u = if t == u then cs else unsatisfiable oc
lpo' strict oc r cs t@(App f ts) u@(App g us) = result
  where
    result  = intersect oc cs result'
    result' = unionAll oc [case1, case2, case3]

    -- tᵢ ≥ u for some i
    case1 = unionAll oc (map go ts) where
      go ti = lpo' strict oc GTE cs ti u

    -- f > g ∧ t > uⱼ for all j
    case2 =
      if f == g
      then unsatisfiable oc
      else intersect oc tDominatesUs (singleton oc $ f >. g)

    -- f = g ∧ t > uⱼ for all j ts >lex us
    case3 =
      if strict && f /= g
      then unsatisfiable oc
      else intersectAll oc ([tDominatesUs, lex oc (r == GT) cs (lpo' strict) ts us] ++ symEQ) where
        symEQ = [singleton oc (f =. g) | f /= g]


    tDominatesUs = intersectAll oc (map go us) where
      go = lpo' strict oc GT cs t


-- | Constraint generator for a quasi-order extension to the Lexicographic path ordering
lpo :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RuntimeTerm Identity
lpo oc r cs t u = return $ lpo' False oc r cs t u

-- | Constraint generator for a strict version of the quasi-order extension to
--   the Lexicographic path ordering.
lpoStrict :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RuntimeTerm Identity
lpoStrict oc r cs t u = return $ lpo' True oc r cs t u
