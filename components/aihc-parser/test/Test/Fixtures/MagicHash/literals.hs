{- ORACLE_TEST
id: literals
category: expressions
expected: pass
reason: magic hash literals
-}
{-# LANGUAGE MagicHash #-}
module Literals where

c# = 'a'#
s# = "hello"#
