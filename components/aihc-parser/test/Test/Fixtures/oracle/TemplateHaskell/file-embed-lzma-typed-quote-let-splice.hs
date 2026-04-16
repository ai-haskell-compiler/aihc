{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskellQuotes #-}

module FileEmbedLzmaTypedQuoteLetSplice where

import Language.Haskell.TH.Syntax (Code, Q)

x :: Code Q Int
x = [||1||]

f =
  [||
  let embedded = $$(x)
   in embedded
  ||]
