{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module M where

x = [| Proxy :: Proxy $a |]
