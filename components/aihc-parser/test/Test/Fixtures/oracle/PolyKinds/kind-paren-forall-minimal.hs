{- ORACLE_TEST pass -}
{-# LANGUAGE PolyKinds, ExplicitForAll #-}

module KindParenForall where

f :: forall k (a :: k). a
f = undefined
