{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module TypeSynonymOperatorKindedParams where

type (a :: k1) <= (b :: k2) = (a <=? b) ~ 'True
