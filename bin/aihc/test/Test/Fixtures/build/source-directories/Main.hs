module Main where

import BlackholeChecks (blackholeChecks)
import FrozenChecks (frozenChecks)
import KindOrderChecks (kindOrderChecks)
import PrimitiveChecks (primitiveChecks)
import STMChecks (stmChecks)
import Message
import System.Environment (getArgs)
import System.IO ()

main :: IO ()
main = do
  blackholes <- blackholeChecks
  transactions <- stmChecks
  if blackholes && primitiveChecks && transactions && frozenChecks && kindOrderChecks then run else error "primitive check failed"

run :: IO ()
run = do
  arguments <- getArgs
  case arguments of
    [] -> putStrLn message
    [first, second] -> do
      putStrLn first
      putStrLn second
    _ -> putStrLn "unexpected arguments"
