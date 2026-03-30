{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (resultOutput)
import Aihc.Parser (ParseResult (..))
import qualified Aihc.Parser
import Aihc.Parser.Syntax (Module)
import CppSupport (preprocessForParserWithoutIncludes)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as Utf8
import ExtensionSupport
import qualified GHC.LanguageExtensions.Type as GHC
import GhcOracle
  ( extensionNamesToGhcExtensions,
    oracleModuleAstFingerprintWithExtensionsAt,
    oracleParsesModuleWithExtensionsAt,
  )
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args

  cases <- loadOracleCases
  outcomes <- mapM evaluateCase cases

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

evaluateCase :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase meta = do
  source <- Utf8.readFile (caseSourcePath meta)
  let exts = extensionNamesToGhcExtensions (caseExtensions meta) Nothing
      source' = resultOutput (preprocessForParserWithoutIncludes (casePath meta) source)
      parsed = Aihc.Parser.parseModule Aihc.Parser.defaultConfig source'
      oracleOk = oracleParsesModuleWithExtensionsAt "parser-progress" exts source'
      roundtripOk = moduleRoundtripsViaGhc exts source' parsed
      (outcome, details) = classifyForCase meta oracleOk roundtripOk
  pure (meta, outcome, details)

classifyForCase :: CaseMeta -> Bool -> Bool -> (Outcome, String)
classifyForCase meta oracleOk roundtripOk
  | "haskell2010/" `isPrefixOf` casePath meta =
      case expected of
        ExpectPass
          | not oracleOk -> (OutcomeFail, "oracle rejected pass case")
          | not roundtripOk -> (OutcomeFail, "roundtrip mismatch against oracle AST")
          | otherwise -> (OutcomePass, "")
        ExpectXFail
          | oracleOk && roundtripOk -> (OutcomeXPass, "case now passes oracle and roundtrip checks")
          | otherwise -> (OutcomeXFail, "")
  | otherwise = classifyOutcome expected oracleOk roundtripOk
  where
    expected = caseExpected meta

moduleRoundtripsViaGhc :: [GHC.Extension] -> Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhc exts source oursResult =
  case oursResult of
    ParseErr _ -> False
    ParseOk parsed ->
      let rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty parsed))
       in case ( oracleModuleAstFingerprintWithExtensionsAt "parser-progress" exts source,
                 oracleModuleAstFingerprintWithExtensionsAt "parser-progress" exts rendered
               ) of
            (Right sourceAst, Right renderedAst) -> sourceAst == renderedAst
            _ -> False

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
