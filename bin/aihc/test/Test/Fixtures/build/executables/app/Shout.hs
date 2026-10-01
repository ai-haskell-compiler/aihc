module Main (main) where

-- Bang has no declarations, so its object file is empty.
import Bang (String)

main :: IO ()
main = putStrLn (shout "build")
  where
    shout = \case
      [] -> "!"
      name -> name ++ "!"
