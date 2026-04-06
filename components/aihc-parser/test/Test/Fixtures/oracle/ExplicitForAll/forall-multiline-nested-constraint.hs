{- ORACLE_TEST xfail data-compat parenthesized constraint family application in context -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ForallMultilineNestedConstraint where

import Data.Kind (Constraint, Type)

type family D a :: Type -> Constraint

f :: ((D a) a) => ()
f = ()
