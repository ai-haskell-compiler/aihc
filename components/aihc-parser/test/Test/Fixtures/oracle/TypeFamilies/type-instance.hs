{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module TypeInstance where

type family Elem c
type instance Elem [e] = e