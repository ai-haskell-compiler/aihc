{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, StandaloneKindSignatures #-}

module InlineKindSignatureNewtypeTraditional where

import Data.Kind (Type)

newtype T :: Type -> Type where
  MkT :: ()
