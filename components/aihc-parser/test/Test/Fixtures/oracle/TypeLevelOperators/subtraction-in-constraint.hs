{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}

import GHC.TypeNats

f :: KnownNat (n - m) => proxy n -> proxy m
f = undefined
