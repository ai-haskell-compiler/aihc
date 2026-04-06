{- ORACLE_TEST xfail webgear-swagger-ui unquoted DataKinds list type -}
{-# LANGUAGE DataKinds #-}
module UnquotedListType where

type T = [Int, Bool]
