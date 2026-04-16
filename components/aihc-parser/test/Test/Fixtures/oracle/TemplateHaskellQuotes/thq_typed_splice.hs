{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskellQuotes #-}

module THQ_Typed_Splice where

import Language.Haskell.TH.Syntax (Code, Q)

x :: Code Q Int
x = $$x
