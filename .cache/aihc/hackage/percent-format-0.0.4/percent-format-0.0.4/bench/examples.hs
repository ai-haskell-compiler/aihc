-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Text.PercentFormat
import Test.LeanCheck
import System.Environment (getArgs)

put :: String -> IO ()
put s  =  putStrLn . unlines
       $  (s %&) `map` take 12 (list :: [String])
       ++ (s %&) `map` take 12 (list :: [Int])
       ++ (s %&) `map` take 12 (list :: [Float])
       ++ (s %&) `map` take 12 (list :: [Rational])
       ++ (s %&) `map` take 12 (list :: [Char])
       ++ (s %&) `map` take 12 (list :: [Bool])

main :: IO ()
main = do
  as <- getArgs
  let s = case as of
          [] -> "%r"
          (s:_) -> s
  put s

(%&) :: Show a => String -> a -> String
s %& x = "%r -%% %12-s == %r" % s % show11 x -% (s -% x)

show11 :: Show a => a -> String
show11 x = showsPrec 11 x ""
