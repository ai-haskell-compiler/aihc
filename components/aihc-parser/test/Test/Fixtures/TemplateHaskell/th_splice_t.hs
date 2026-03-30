{- ORACLE_TEST
id: th_splice_t
category: types
expected: xfail
reason: TemplateHaskell $t syntax
-}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_T where

type T = $typ
type U = $(typ arg)
