{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TypeQuoteSpliceTypeApplication where

import Language.Haskell.TH

data a := b

f c v = [t|$c := $v|]
