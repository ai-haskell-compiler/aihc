{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module DomainAesonQualifiedOperatorNameQuote where

import qualified Prelude as P

f required =
  if required
    then '(P.+)
    else '(P.-)
