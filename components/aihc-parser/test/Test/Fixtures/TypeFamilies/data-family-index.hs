{- ORACLE_TEST
id: data-family-index
category: declarations
expected: xfail
reason: data family with explicit kind
-}
{-# LANGUAGE TypeFamilies #-}
module DataFamilyIndex where

import Data.Kind (Type)

data family GMap k :: Type -> Type
