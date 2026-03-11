module Main (main) where

import qualified Test.Lazy as Lazy
import qualified Test.Strict as Strict
import Text.Printf (printf)


run :: String -> [(String, IO ())] -> IO ()
run prefix tests =
   mapM_ (\(s,a) -> printf "%-25s: " (prefix ++ "." ++ s) >> a) tests

main :: IO ()
main = do
   run "SV" Strict.vp_tests
   run "SVL" Lazy.tests
