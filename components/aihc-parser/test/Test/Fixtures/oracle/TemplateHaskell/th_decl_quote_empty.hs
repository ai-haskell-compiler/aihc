{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Decl_Quote_Empty where

import Language.Haskell.TH (Dec, Q)

-- Empty declaration quotes are valid syntax
emptyDecls :: Q [Dec]
emptyDecls = [d| |]
