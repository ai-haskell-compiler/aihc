module Main (main) where

main :: IO ()
main = putStrLn (shout "build")
  where
    shout = \case
      [] -> "!"
      name -> name ++ "!"
