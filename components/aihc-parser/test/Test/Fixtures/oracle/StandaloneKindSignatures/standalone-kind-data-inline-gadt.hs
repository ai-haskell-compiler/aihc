{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, StandaloneKindSignatures #-}

module InlineKindSignatureData where

import Data.Kind (Type)

data T :: Type -> Type where
  MkT :: a -> T a
