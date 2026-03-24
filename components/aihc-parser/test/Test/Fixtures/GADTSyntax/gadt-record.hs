{-# LANGUAGE GADTSyntax #-}

module GadtRecord where

data Person where
    Adult :: { name :: String, children :: [Person] } -> Person
    Child :: Show a => { name :: !String, funny :: a } -> Person
