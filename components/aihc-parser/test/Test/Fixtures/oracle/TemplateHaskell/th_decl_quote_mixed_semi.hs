{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module TH_Decl_Quote_Mixed_Semi where

import Language.Haskell.TH (Dec, Q)

-- Mixed explicit semicolons and newlines
mkDecls :: Q [Dec]
mkDecls =
  [d|
    x :: Int
    x = 1

    y :: Int
    y = 2
    |]
