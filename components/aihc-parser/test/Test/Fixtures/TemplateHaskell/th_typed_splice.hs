{- ORACLE_TEST
id: th_typed_splice
category: expressions
expected: xfail
reason: TemplateHaskell $$e syntax
-}
{-# LANGUAGE TemplateHaskell #-}
module TH_Typed_Splice where

x = $$expr
y = $$(expr arg)
