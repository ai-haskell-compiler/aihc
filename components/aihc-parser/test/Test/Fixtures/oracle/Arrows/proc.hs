{- ORACLE_TEST xfail basic proc expression -}
{-# LANGUAGE Arrows #-}
module Proc where

f g = proc x -> g -< x