{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Operator_Splice_E where

x = $(&&)
y = $(+)
