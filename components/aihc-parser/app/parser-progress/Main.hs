{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ExtensionSupport
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args

  cases <- loadOracleCases
  outcomes <- mapM evaluateCaseFromFile cases

  let passN = countOutcome OutcomePass outcomes
      xfailN = countOutcome OutcomeXFail outcomes
      xpassN = countOutcome OutcomeXPass outcomes
      failN = countOutcome OutcomeFail outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct (passN + xpassN) totalN

  putStrLn "Parser progress"
  putStrLn "==============="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  let regressions = [(meta, details) | (meta, OutcomeFail, details) <- outcomes]
      xpasses = [(meta, details) | (meta, OutcomeXPass, details) <- outcomes]

  mapM_ printRegression regressions
  mapM_ printXPass xpasses

  if null regressions && (not strict || null xpasses)
    then exitSuccess
    else exitFailure

countOutcome :: Outcome -> [(CaseMeta, Outcome, String)] -> Int
countOutcome target = length . filter (\(_, outcome, _) -> outcome == target)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

printRegression :: (CaseMeta, String) -> IO ()
printRegression (meta, details) =
  putStrLn
    ( "FAIL "
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )

printXPass :: (CaseMeta, String) -> IO ()
printXPass (meta, details) =
  putStrLn
    ( "XPASS "
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )
