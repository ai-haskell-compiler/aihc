{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module TH_Decl_Quote_Multi_Newlines where

import Language.Haskell.TH (Dec, Q)

-- Multiple declarations separated by newlines (layout)
mkDecls :: Q [Dec]
mkDecls =
  [d|
    foo :: Int -> Int
    foo x = x + 1

    bar :: String
    bar = "hello"
    |]
