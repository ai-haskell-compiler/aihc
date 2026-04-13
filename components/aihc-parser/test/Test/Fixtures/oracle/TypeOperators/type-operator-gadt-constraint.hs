{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TypeLevelKVList where

data (:=) key v

data KVList xs where
  KVCons :: KnownSymbol key => key := v -> KVList xs -> KVList ((key := v) ': xs)
