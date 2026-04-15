{- ORACLE_TEST xfail reason="a following equation with a quoted empty-list TH name is rejected after a quoted arrow-name equation" -}
{-# LANGUAGE TemplateHaskell #-}

module X where

import Language.Haskell.TH

headOfType ArrowT = ''(->)
headOfType ListT = ''[]
