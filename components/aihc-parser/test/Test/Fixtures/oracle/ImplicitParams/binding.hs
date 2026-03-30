{- ORACLE_TEST xfail implicit parameter binding -}
{-# LANGUAGE ImplicitParams #-}
module Binding where

f = ?x where ?x = 10