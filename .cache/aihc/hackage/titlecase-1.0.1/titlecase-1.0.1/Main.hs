module Main ( main ) where

import Data.Text.Titlecase
import System.Environment

main :: IO ()
main = getArgs >>= parseArgs >>= putStrLn . titlecase
  where parseArgs :: [String] -> IO String
        parseArgs [] = getContents
        parseArgs xs = return $ unwords xs
