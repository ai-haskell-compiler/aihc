{- ORACLE_TEST xfail GADT constructor with type operator after constraint -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TypeLevelKVList where

data (:=) key v

data KVList xs where
  KVCons :: KnownSymbol key => key := v -> KVList xs -> KVList ((key := v) ': xs)
