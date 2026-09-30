module Loud (shout) where

shout :: String -> String
shout = \case
  [] -> "!"
  name -> name ++ "!"
