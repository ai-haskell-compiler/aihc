{- ORACLE_TEST xfail TemplateHaskell $t syntax -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_T where

type T = $typ
type U = $(typ arg)