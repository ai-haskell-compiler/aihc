{- ORACLE_TEST
id: default-type-instance
category: declarations
expected: xfail
reason: default type instance
-}
{-# LANGUAGE TypeFamilies #-}
module DefaultTypeInstance where

class IsBoolMap v where
  type Key v
  type instance Key v = Int
