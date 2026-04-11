{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module TH_Decl_Quote_In_Do where

import Language.Haskell.TH (Dec, Q)

-- Declaration quote inside a do block
mkDecls :: Q [Dec]
mkDecls = do
  let x = 1
  [d|
    val :: Int
    val = 42
    |]
