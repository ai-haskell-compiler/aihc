{- ORACLE_TEST xfail TemplateHaskell $$e syntax -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Typed_Splice where

x = $$expr
y = $$(expr arg)