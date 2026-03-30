{- ORACLE_TEST
id: standalone-kind-nullary
category: declarations
expected: pass
-}
{-# LANGUAGE StandaloneKindSignatures #-}

module StandaloneKindNullary where

import Data.Kind (Type)

type Range :: Type
data Range = Range
