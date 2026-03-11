module Main where

import qualified Sound.MED.Generic as MED
import Sound.MED.Basic.Human(human)

import System.Environment(getArgs)

main :: IO ()
main = mapM_ printMED =<< getArgs

printMED :: String -> IO ()
printMED file =
  putStr . human =<< MED.load file
