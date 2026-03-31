{- ORACLE_TEST xfail wildcard in return type -}
{-# LANGUAGE PartialTypeSignatures #-}
module Just1 where

just1 :: _ Int
just1 = Just 1
