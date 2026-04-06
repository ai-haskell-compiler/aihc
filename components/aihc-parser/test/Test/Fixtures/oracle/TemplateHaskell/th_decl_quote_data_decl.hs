{- ORACLE_TEST xfail Template Haskell declaration quotes reject non-value declarations -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Decl_Quote_Data_Decl where

import Language.Haskell.TH (Dec, Q)

decl :: Q [Dec]
decl = [d| data Nat = Z |]
