{- ORACLE_TEST xfail Parens.addModuleParens changes the parsed snippet -}
{-# LANGUAGE TemplateHaskell #-}
module M where

x = [d| instance X $a |]
