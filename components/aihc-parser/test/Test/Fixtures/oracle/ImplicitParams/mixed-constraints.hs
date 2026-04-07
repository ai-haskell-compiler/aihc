{- ORACLE_TEST pass -}
{-# LANGUAGE ImplicitParams #-}
module ImplicitParams.MixedConstraints where

-- | Test implicit parameters mixed with type class constraints
x :: (?v :: Int, Show a) => a -> String
x val = show (val, ?v)
