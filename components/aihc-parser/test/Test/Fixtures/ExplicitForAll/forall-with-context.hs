{- ORACLE_TEST
id: forall-with-context
category: types
expected: pass
-}
{-# LANGUAGE ExplicitForAll #-}

module ExplicitForAllWithContext where

render :: forall a. Show a => a -> String
render = show
