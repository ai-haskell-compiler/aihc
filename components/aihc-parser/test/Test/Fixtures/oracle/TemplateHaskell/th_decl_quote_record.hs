{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Decl_Quote_Record where

import Language.Haskell.TH (Dec, Q)

decl :: Q [Dec]
decl = [d|
  data Person = Person
    { name :: String
    , age :: Int
    }
  |]
