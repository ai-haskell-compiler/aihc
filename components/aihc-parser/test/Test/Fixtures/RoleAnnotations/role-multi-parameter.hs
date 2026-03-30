{- ORACLE_TEST
id: role-multi-parameter
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE RoleAnnotations #-}

module RoleMultiParameter where

type role MultiBox representational phantom
data MultiBox a b = MultiBox a
