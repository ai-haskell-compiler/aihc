{- ORACLE_TEST xfail parser support pending -}
{-# LANGUAGE RoleAnnotations #-}

module RoleSingleParameter where

type role NominalBox nominal
data NominalBox a = NominalBox a