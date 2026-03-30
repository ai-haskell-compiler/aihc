{- ORACLE_TEST
id: empty
category: expressions
expected: xfail
reason: empty multiline string
-}
{-# LANGUAGE MultilineStrings #-}
module Empty where

s :: String
s = """"""
