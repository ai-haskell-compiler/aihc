{- ORACLE_TEST
id: explicit-namespaces-export-type
category: modules
expected: pass
reason: parser now supports type namespace export items
-}
{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesExportType (type Token(..), makeToken) where

data Token = Token

makeToken :: Token
makeToken = Token
