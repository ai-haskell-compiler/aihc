{- ORACLE_TEST xfail reason="typed Template Haskell quote with let-bound typed splice is not parsed" -}
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
