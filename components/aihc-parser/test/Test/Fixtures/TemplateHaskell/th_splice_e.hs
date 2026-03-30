{- ORACLE_TEST xfail TemplateHaskell $e syntax -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_E where

x = $expr
y = $(expr arg)