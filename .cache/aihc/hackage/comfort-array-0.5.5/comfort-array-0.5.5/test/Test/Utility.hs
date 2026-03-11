module Test.Utility where

prefix :: String -> [(String, test)] -> [(String, test)]
prefix msg =
   map (\(str,test) -> (msg ++ "." ++ str, test))
