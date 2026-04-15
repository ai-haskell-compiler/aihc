{- ORACLE_TEST xfail reason="qualified operator name quotes in Template Haskell are not parsed" -}
{-# LANGUAGE TemplateHaskell #-}

module DomainAesonQualifiedOperatorNameQuote where

import qualified Prelude as P

f required =
  if required
    then '(P.+)
    else '(P.-)
