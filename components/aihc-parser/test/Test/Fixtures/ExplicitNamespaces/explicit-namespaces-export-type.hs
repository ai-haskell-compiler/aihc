{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesExportType (type Token(..), makeToken) where

data Token = Token

makeToken :: Token
makeToken = Token