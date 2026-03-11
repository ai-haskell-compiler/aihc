{-# LANGUAGE UndecidableInstances #-}

{- | Relational predicate simplification.

These bits aren't mutually recursive with the main simplifier, so we can keep
them separate for a bit of code hygiene.

__Internal module. Exports may change without warning. Try not to use.__
-}

module Rerefined.Simplify.Relational where

import Rerefined.Predicate.Succeed
import Rerefined.Predicate.Fail

import Rerefined.Predicate.Relational
import GHC.TypeNats ( Natural, CmpNat )
import Data.Type.Ord ( OrdCond )

type family SimplifyCompareLength (op :: RelOp) (n :: Natural) where
    SimplifyCompareLength RelOpLT  0 = Just Fail
    SimplifyCompareLength RelOpLTE 0 = Just (CompareLength RelOpEQ 0)
    SimplifyCompareLength RelOpGTE 0 = Just Succeed
    SimplifyCompareLength RelOpGT  0 = Just (CompareLength RelOpEQ 0)

    SimplifyCompareLength op n = Nothing

type family SimplifyCompareLengthAnd
  (lop :: RelOp) (ln :: Natural) (rop :: RelOp) (rn :: Natural) where
    -- @<n AND >m@: @n<=m@ -> False
    SimplifyCompareLengthAnd RelOpLT  n RelOpGT  m =
        OrdCond (CmpNat n m)
            (Just Fail)
            (Just Fail)
            Nothing
    SimplifyCompareLengthAnd RelOpGT  m RelOpLT  n =
        OrdCond (CmpNat n m)
            (Just Fail)
            (Just Fail)
            Nothing

    SimplifyCompareLengthAnd lop ln rop rn = Nothing

type family SimplifyCompareLengthOr
  (lop :: RelOp) (ln :: Natural) (rop :: RelOp) (rn :: Natural) where
    -- @<n OR  >m@: @n==m@ -> NEQ; @n>m@ -> True (?)
    SimplifyCompareLengthOr RelOpLT  n RelOpGT  m =
        OrdCond (CmpNat n m)
            Nothing
            (Just (CompareLength RelOpNEQ n))
            (Just Succeed)
    SimplifyCompareLengthOr RelOpGT  m RelOpLT  n =
        OrdCond (CmpNat n m)
            Nothing
            (Just (CompareLength RelOpNEQ n))
            (Just Succeed)

    SimplifyCompareLengthOr lop ln rop rn = Nothing
