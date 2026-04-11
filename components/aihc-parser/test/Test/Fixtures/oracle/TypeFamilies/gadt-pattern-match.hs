{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module LiftTypeRepro where

typeRepToType :: SomeTypeRep -> Type
typeRepToType (SomeTypeRep a) = go a
  where
    go :: Type
    go = undefined
