{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Language.Haskell.TH (integerL, litE)

main :: IO ()
main = print ($(litE (integerL 42)) :: Integer)
