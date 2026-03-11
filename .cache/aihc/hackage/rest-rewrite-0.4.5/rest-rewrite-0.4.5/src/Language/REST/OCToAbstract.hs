{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.REST.OCToAbstract where

import Data.Hashable

import Control.Monad.Identity

import Language.REST.OCAlgebra
import qualified Language.REST.WQOConstraints as OC
import Language.REST.Types
import Language.REST.SMT (ToSMTVar)

-- | @lift@ takes a representation of constraints on a WQO over @base@,
--   alongside a function used to generate constraints to permit a relation on terms @lifted@,
--   and returns the corresponding Ordering Constraints Algebra
lift :: forall impl base lifted m . (ToSMTVar base Int, Ord base, Eq base, Hashable base, Show lifted, Show base, Show (impl base)) =>
     OC.WQOConstraints impl m
  -> OC.ConstraintGen impl base lifted Identity
  -> OCAlgebra (impl base) lifted m
lift oc cgen =
  OCAlgebra {
    isSat  = isSat'
  , top    = top'
  , refine = refine'
  , union  = OC.union oc
  , notStrongerThan = OC.notStrongerThan oc
  }
  where
    isSat' :: impl base -> m Bool
    isSat' = OC.isSatisfiable oc

    top' :: impl base
    top' = OC.noConstraints oc

    refine' :: impl base -> lifted -> lifted -> impl base
    refine' c t u =
      let
        pair   = runIdentity $ cgen oc GTE top' t u
        result = OC.intersect oc c pair
      in
        result
