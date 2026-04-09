{- ORACLE_TEST xfail reason="TemplateHaskell declaration quote with type signature not handled" -}
{-# LANGUAGE TemplateHaskell #-}
module THDeclQuoteWithSig where

import Language.Haskell.TH

mkX0 :: DecsQ
mkX0 = [d|
        x :: s -> b
        x = undefined
        |]
