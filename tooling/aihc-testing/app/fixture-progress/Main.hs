-- | Progress reporting for the desugaring and code generation stages.
--
-- Counts the fixtures of one progress group and outputs a summary in the
-- standard PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE format that the resolver and
-- type checker progress tools use. Nothing is compiled or evaluated: the
-- counts come from the status each fixture declares for itself, so FAIL is
-- always zero here. The test suite is what catches a fixture that does not
-- do what it declares.
--
-- A fixture with status @fail@ asserts an expected error, so it counts as a
-- pass. Only the known-bug markers @xfail@ and @xpass@ describe unimplemented
-- behaviour.
module Main (main) where

import Aihc.Testing.FixtureIndex
  ( FixtureEntry (..),
    FixtureGroup,
    FixtureStatus (..),
    allFixtureGroups,
    groupName,
    groupSuites,
    loadFixtureSuites,
    suiteName,
  )
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args
      names = filter (/= "--strict") args
  group <- case names of
    [name] | Just group <- lookup name groupsByName -> pure group
    _ -> do
      hPutStrLn stderr ("Usage: fixture-progress [--strict] <" <> intercalate "|" (map fst groupsByName) <> ">")
      exitFailure
  entries <- loadFixtureSuites (groupSuites group)

  let count status = length (filter ((== status) . entryStatus) entries)
      passN = count StatusPass + count StatusFail
      xfailN = count StatusXFail
      xpassN = count StatusXPass
      failN = 0 :: Int
      totalN = length entries
      completion = pct (passN + xpassN) totalN

  putStrLn (title group)
  putStrLn (replicate (length (title group)) '=')
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  mapM_ (\entry -> putStrLn ("XFAIL " <> suiteName (entrySuite entry) <> " " <> entryPath entry)) (filter ((== StatusXFail) . entryStatus) entries)
  mapM_ (\entry -> putStrLn ("XPASS " <> suiteName (entrySuite entry) <> " " <> entryPath entry)) (filter ((== StatusXPass) . entryStatus) entries)

  if not strict || xpassN == 0
    then exitSuccess
    else exitFailure

groupsByName :: [(String, FixtureGroup)]
groupsByName = [(groupName group, group) | group <- allFixtureGroups]

title :: FixtureGroup -> String
title group = "Fixture progress: " <> groupName group

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0
