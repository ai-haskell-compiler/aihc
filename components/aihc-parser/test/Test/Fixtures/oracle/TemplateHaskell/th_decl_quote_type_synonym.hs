{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Decl_Quote_Type_Synonym where

import Language.Haskell.TH (Dec, Q)

decl :: Q [Dec]
decl = [d| type StringList = [String] |]
