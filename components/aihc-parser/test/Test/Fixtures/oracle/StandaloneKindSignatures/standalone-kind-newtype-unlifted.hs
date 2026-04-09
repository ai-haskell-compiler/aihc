{- ORACLE_TEST xfail inline kind signature on GADT-style newtype -}
{-# LANGUAGE DataKinds, StandaloneKindSignatures #-}

module InlineKindSignatureGADT where

newtype T :: TYPE 'WordRep where
  MkT :: ()
