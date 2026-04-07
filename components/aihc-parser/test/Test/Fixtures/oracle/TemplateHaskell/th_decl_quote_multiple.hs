{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Decl_Quote_Multiple where

import Language.Haskell.TH (Dec, Q)

decl :: Q [Dec]
decl = [d| data Nat = Z | S Nat |]
