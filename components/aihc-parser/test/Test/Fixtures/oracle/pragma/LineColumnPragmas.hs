{- ORACLE_TEST pass -}
module LineColumnPragmas where

{-# LINE 42 "GeneratedFrom.dsl" #-}

lineTagged :: Int
lineTagged =
  do {-# COLUMN 17 #-}
     pure 1
