{- ORACLE_TEST xfail type-level subtraction operator in constraint fails to parse -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}

import GHC.TypeNats

f :: KnownNat (n - m) => proxy n -> proxy m
f = undefined
