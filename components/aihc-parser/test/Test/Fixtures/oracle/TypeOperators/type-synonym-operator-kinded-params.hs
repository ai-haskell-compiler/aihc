{- ORACLE_TEST xfail kind annotations on type synonym operator parameters are dropped during roundtrip -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module TypeSynonymOperatorKindedParams where

type (a :: k1) <= (b :: k2) = (a <=? b) ~ 'True
