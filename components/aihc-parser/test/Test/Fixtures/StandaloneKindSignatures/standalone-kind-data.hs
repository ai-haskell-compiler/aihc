{- ORACLE_TEST
id: standalone-kind-data
category: declarations
expected: pass
-}
{-# LANGUAGE StandaloneKindSignatures #-}

module StandaloneKindData where

import Data.Kind (Type)

type Box :: Type -> Type
data Box a = Box a
