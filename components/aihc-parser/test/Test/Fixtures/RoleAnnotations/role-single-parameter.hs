{- ORACLE_TEST
id: role-single-parameter
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE RoleAnnotations #-}

module RoleSingleParameter where

type role NominalBox nominal
data NominalBox a = NominalBox a
