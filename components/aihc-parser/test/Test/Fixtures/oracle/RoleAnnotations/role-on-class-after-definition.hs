{- ORACLE_TEST pass -}
{-# LANGUAGE RoleAnnotations #-}

module RoleOnClassAfterDefinition where

class C a b where
  method :: a -> b -> ()

type role C representational _
