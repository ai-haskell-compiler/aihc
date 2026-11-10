{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Language.Haskell.TH

main :: IO ()
main = do
  print ($(litE (integerL 42)) :: Int)
  print ($(appE (varE (mkName "negate")) (litE (integerL 7))) + 1 :: Int)
  putStrLn $(litE (stringL (concat (replicate 3 "ab"))))
