{-# LANGUAGE TemplateHaskell #-}

-- | A module of the program that a splice of "Main" uses.
module Helper (helperValue, plusHelper) where

import Language.Haskell.TH

helperValue :: Int
helperValue = 6

-- | A quote that names a value of this module.
plusHelper :: Int -> Q Exp
plusHelper n = [|helperValue + n|]
