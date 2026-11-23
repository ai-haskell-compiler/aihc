module Main where

import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (hClose, hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  copy <- hDuplicate stdout
  hPutStrLn copy "written through the duplicate"
  hClose copy
  putStrLn "stdout stays open after the duplicate is closed"
  hDuplicateTo stderr stdout
  putStrLn "stdout refers to stderr after hDuplicateTo"
