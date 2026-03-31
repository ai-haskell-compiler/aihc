{- ORACLE_TEST xfail wildcard in let binding -}
{-# LANGUAGE PartialTypeSignatures #-}
module LetBinding where

x = let y :: _; y = False in y
