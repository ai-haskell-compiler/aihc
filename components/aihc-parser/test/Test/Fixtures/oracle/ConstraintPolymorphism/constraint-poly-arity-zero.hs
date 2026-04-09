{- ORACLE_TEST xfail parser rejects tilde in constrained type with zero-arity constraint -}
{-# LANGUAGE GHC2021 #-}

f :: (() ~ () => a) -> a
f x = x
