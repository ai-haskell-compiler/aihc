{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module X where

import Language.Haskell.TH

headOfType ArrowT = ''(->)
headOfType ListT = ''[]
