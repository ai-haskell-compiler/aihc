{- ORACLE_TEST xfail parser support pending -}
{-# LANGUAGE RoleAnnotations #-}

module RoleOnNewtype where

type role Wrap nominal
newtype Wrap a = Wrap a