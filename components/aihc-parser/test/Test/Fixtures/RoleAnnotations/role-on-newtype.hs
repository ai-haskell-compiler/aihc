{- ORACLE_TEST
id: role-on-newtype
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE RoleAnnotations #-}

module RoleOnNewtype where

type role Wrap nominal
newtype Wrap a = Wrap a
