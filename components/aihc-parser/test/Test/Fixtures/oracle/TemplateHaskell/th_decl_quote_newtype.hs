{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Decl_Quote_Newtype where

import Language.Haskell.TH (Dec, Q)

decl :: Q [Dec]
decl = [d| newtype Wrapper a = Wrapper a |]
