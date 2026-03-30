{- ORACLE_TEST
id: type-instance
category: declarations
expected: xfail
reason: type instance
-}
{-# LANGUAGE TypeFamilies #-}
module TypeInstance where

type family Elem c
type instance Elem [e] = e
