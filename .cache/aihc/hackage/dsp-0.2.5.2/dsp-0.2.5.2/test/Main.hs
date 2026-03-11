module Main where

import qualified Test.Matrix.QR as QR


prefix :: String -> [(String, IO ())] -> [(String, IO ())]
prefix msg =
   map (\(str,test) -> (msg ++ "." ++ str, test))

main :: IO ()
main =
   mapM_ (\(msg,io) -> putStr (msg++": ") >> io) $
   concat $
      prefix "Matrix.QR" QR.tests :
      []
