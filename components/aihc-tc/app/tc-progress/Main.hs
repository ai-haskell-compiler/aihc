{-# LANGUAGE OverloadedStrings #-}

-- | Progress reporting for the type checker.
--
-- Loads YAML fixtures from the golden test directory, evaluates
-- each case, and outputs a summary in the standard
-- PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE format.
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import TcGolden

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args

  cases <- loadTcCases
  let evaluated = [(tc, outcome, details) | tc <- cases, let (outcome, details) = evaluateTcCase tc]
      (passN, xfailN, xpassN, failN) = progressSummary evaluated
      totalN = passN + xfailN + xpassN + failN
      completion = pct (passN + xpassN) totalN

  putStrLn "Type checker progress"
  putStrLn "====================="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  let failures = [(tc, details) | (tc, OutcomeFail, details) <- evaluated]
      xpasses = [(tc, details) | (tc, OutcomeXPass, details) <- evaluated]

  mapM_ printFailure failures
  mapM_ printXPass xpasses

  if null failures && (not strict || null xpasses)
    then exitSuccess
    else exitFailure

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

printFailure :: (TcCase, String) -> IO ()
printFailure (tc, details) =
  putStrLn
    ( "FAIL "
        <> caseId tc
        <> " ["
        <> caseCategory tc
        <> "] "
        <> details
    )

printXPass :: (TcCase, String) -> IO ()
printXPass (tc, details) =
  putStrLn
    ( "XPASS "
        <> caseId tc
        <> " ["
        <> caseCategory tc
        <> "] "
        <> details
    )
