{- ORACLE_TEST
id: basic
category: expressions
expected: pass
reason: basic magic hash identifiers
-}
{-# LANGUAGE MagicHash #-}
module Basic where

x# :: Int#
x# = 42#
