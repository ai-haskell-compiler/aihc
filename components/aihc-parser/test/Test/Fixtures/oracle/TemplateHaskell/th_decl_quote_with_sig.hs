{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module THDeclQuoteWithSig where

import Language.Haskell.TH

mkX0 :: DecsQ
mkX0 =
  [d|
    x :: s -> b
    x = undefined
    |]
