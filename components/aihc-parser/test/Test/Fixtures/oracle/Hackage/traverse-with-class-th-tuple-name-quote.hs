{- ORACLE_TEST xfail reason="template haskell type-name quotes reject tuple constructors" -}
{-# LANGUAGE TemplateHaskell #-}

module TraverseWithClassTHTupleNameQuote where

f = ''(,)
