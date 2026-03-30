{- ORACLE_TEST
id: forall-nested-arrow
category: types
expected: pass
-}
{-# LANGUAGE ExplicitForAll #-}

module ExplicitForAllNestedArrow where

apply :: (forall a. a -> a) -> Int
apply f = f 3
