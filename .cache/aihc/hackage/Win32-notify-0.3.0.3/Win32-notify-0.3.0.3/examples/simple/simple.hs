module Main where

import System.Directory
import System.Win32.Notify

main :: IO ()
main = do
  watchManager <- initWatchManager
  home <- getHomeDirectory
  wd <- watchDirectory watchManager home isRecursive varieties handler
  print wd
  putStrLn "Listens to your home directory. Hit enter to terminate."
  _ <- getLine -- TODO: This hangs... why is that?
  killWatchManager watchManager
  putStrLn "Quitting."
  where
    isRecursive = False
    varieties = [Create, Delete, Modify, Move]
    handler :: Handler
    handler event = putStrLn $ show event
