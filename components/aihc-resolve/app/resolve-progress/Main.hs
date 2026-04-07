module Main (main) where

import qualified ResolverGolden as RG
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args
  cases <- RG.loadResolverCases
  let outcomes = map evaluate cases
      (passN, xfailN, xpassN, failN) = RG.progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct (passN + xpassN) totalN
  putStrLn "Haskell name resolution progress"
  putStrLn "================================="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  mapM_ printFail [(m, d) | (m, RG.OutcomeFail, d) <- outcomes]
  mapM_ printXPass [(m, d) | (m, RG.OutcomeXPass, d) <- outcomes]

  if failN == 0 && (not strict || xpassN == 0)
    then exitSuccess
    else exitFailure

evaluate :: RG.ResolverCase -> (RG.ResolverCase, RG.Outcome, String)
evaluate meta =
  let (outcome, details) = RG.evaluateResolverCase meta
   in (meta, outcome, details)

printFail :: (RG.ResolverCase, String) -> IO ()
printFail (meta, details) =
  putStrLn ("FAIL " <> RG.caseId meta <> " [" <> RG.caseCategory meta <> "] " <> details)

printXPass :: (RG.ResolverCase, String) -> IO ()
printXPass (meta, details) =
  putStrLn ("XPASS " <> RG.caseId meta <> " [" <> RG.caseCategory meta <> "] " <> details)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0
