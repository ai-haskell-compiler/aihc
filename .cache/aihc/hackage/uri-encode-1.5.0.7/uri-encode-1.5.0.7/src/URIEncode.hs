module Main where

import Data.List (intercalate)
import System.Environment (getArgs)

import Network.URI.Encode

main :: IO ()
main = getArgs >>= \args ->
  if null args
    then interact (unlines . map encode . lines)
    else putStrLn . intercalate " " . map encode $ args


