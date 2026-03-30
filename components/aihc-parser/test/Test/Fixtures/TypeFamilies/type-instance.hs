{- ORACLE_TEST xfail type instance -}
{-# LANGUAGE TypeFamilies #-}
module TypeInstance where

type family Elem c
type instance Elem [e] = e