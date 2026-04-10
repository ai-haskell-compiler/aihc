{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, StandaloneKindSignatures #-}

module InlineKindSignatureGADT where

newtype T :: TYPE 'WordRep where
  MkT :: ()
