{- ORACLE_TEST xfail reason="typed TH splices inside type quotations are rejected after an infix operator" -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TypeQuoteSpliceTypeApplication where

import Language.Haskell.TH

data a := b

f c v = [t|$c := $v|]
