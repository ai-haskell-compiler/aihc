{- ORACLE_TEST pass -}
{-# LANGUAGE ImplicitParams #-}

module StandaloneImplicitParam where

-- Implicit parameter in standalone type position (e.g. type synonym body)
f :: (?x :: Int) => (?y :: Bool) => Int -> Int
f = ?x
