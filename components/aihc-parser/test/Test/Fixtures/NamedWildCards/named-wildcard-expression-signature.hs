{- ORACLE_TEST
id: named-wildcard-expression-signature
category: types
expected: pass
reason: parser now supports expression type signatures
-}
{-# LANGUAGE NamedWildCards #-}

module NamedWildcardExpressionSignature where

identityExpr :: Int
identityExpr = (id :: _b -> _b) 7
