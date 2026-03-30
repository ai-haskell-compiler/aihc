{- ORACLE_TEST
id: associated-type
category: declarations
expected: xfail
reason: associated type family
-}
{-# LANGUAGE TypeFamilies #-}
module AssociatedType where

import Data.Kind (Type)

class Collects ce where
  type Elem ce :: Type
