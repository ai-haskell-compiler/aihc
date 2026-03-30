{- ORACLE_TEST
id: standalone-kind-type-synonym
category: declarations
expected: pass
-}
{-# LANGUAGE StandaloneKindSignatures #-}

module StandaloneKindTypeSynonym where

import Data.Kind (Type)

type Pair :: Type -> Type -> Type
type Pair a b = (a, b)
