{- ORACLE_TEST xfail wildcard in type application -}
{-# LANGUAGE PartialTypeSignatures #-}
module Just2 where

just2 :: Maybe _
just2 = Just False
