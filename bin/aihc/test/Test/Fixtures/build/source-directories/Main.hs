module Main where

import BlackholeChecks (blackholeChecks)
import FamilyBindChecks (familyBindChecks)
import FileInputChecks (fileInputChecks)
import FrozenChecks (frozenChecks)
import KindOrderChecks (kindOrderChecks)
import PrimitiveChecks (primitiveChecks)
import RecordUpdateChecks (recordUpdateChecks)
import ResponseFileChecks (responseFileChecks)
import STMChecks (stmChecks)
import UnpackChecks (unpackChecks)
import SumChecks (sumChecks)
import Message
import System.Environment (getArgs)
import System.IO ()

main :: IO ()
main = do
  responses <- responseFileChecks
  if responses then pure () else error "response file check failed"
  files <- fileInputChecks
  if files then pure () else error "file input check failed"
  blackholes <- blackholeChecks
  transactions <- stmChecks
  if blackholes && primitiveChecks && transactions && frozenChecks && familyBindChecks && kindOrderChecks && sumChecks && recordUpdateChecks && unpackChecks then run else error "primitive check failed"

run :: IO ()
run = do
  arguments <- getArgs
  case arguments of
    [] -> putStrLn message
    [first, second] -> do
      putStrLn first
      putStrLn second
    _ -> putStrLn "unexpected arguments"
