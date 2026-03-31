{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Typed_Splice where

x = $$expr
y = $$(expr arg)