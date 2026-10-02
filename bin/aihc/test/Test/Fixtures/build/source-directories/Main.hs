module Main where

import BlackholeChecks (blackholeChecks)
import FamilyBindChecks (familyBindChecks)
import FileInputChecks (fileInputChecks)
import FrozenChecks (frozenChecks)
import KindOrderChecks (kindOrderChecks)
import PrimitiveChecks (primitiveChecks)
import STMChecks (stmChecks)
import SumChecks (sumChecks)
import Message
import System.Environment (getArgs)
import System.IO ()

main :: IO ()
main = do
  files <- fileInputChecks
  if files then pure () else error "file input check failed"
  blackholes <- blackholeChecks
  transactions <- stmChecks
  if blackholes && primitiveChecks && transactions && frozenChecks && familyBindChecks && kindOrderChecks && sumChecks then run else error "primitive check failed"

run :: IO ()
run = do
  arguments <- getArgs
  case arguments of
    [] -> putStrLn message
    [first, second] -> do
      putStrLn first
      putStrLn second
    _ -> putStrLn "unexpected arguments"
