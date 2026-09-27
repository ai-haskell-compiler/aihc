module Main where

import BlackholeChecks (blackholeChecks)
import FamilyBindChecks (familyBindChecks)
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
