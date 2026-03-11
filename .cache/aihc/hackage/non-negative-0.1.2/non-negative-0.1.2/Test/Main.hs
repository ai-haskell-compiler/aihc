module Main where

import qualified Test.Numeric.NonNegative.Chunky as Chunky

prefix :: String -> [(String, IO ())] -> [(String, IO ())]
prefix msg =
   map (\(str,test) -> (msg ++ "." ++ str, test))

main :: IO ()
main =
   mapM_ (\(msg,io) -> putStr (msg++": ") >> io) $
   concat $
      prefix "NonNegative.Chunky" Chunky.tests :
      []
