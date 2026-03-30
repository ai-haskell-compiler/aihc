{- ORACLE_TEST
id: th_splice_e
category: expressions
expected: xfail
reason: TemplateHaskell $e syntax
-}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_E where

x = $expr
y = $(expr arg)
