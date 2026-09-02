module Data.String
  ( String,
    IsString (..),
    lines,
    words,
    unlines,
    unwords,
  )
where

import Prelude (String, lines, unlines, unwords, words)

class IsString a where
  fromString :: String -> a
