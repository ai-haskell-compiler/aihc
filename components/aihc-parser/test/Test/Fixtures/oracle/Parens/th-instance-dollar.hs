{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module M where

x = [d| instance X $a |]
