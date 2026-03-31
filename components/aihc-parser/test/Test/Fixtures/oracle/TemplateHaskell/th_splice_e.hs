{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_E where

x = $expr
y = $(expr arg)