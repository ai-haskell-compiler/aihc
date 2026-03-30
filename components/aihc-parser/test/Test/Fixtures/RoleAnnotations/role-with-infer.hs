{- ORACLE_TEST
id: role-with-infer
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE RoleAnnotations #-}

module RoleWithInfer where

type role InferBox _ nominal
data InferBox a b = InferBox b
