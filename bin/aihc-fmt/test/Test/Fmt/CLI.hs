{-# LANGUAGE OverloadedStrings #-}

module Test.Fmt.CLI
  ( cliTests,
  )
where

import Aihc.Fmt.CLI
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

cliTests :: TestTree
cliTests =
  testGroup
    "cli"
    [ testCase "formats stdin to stdout" test_stdinStdout,
      testCase "check mode reports unformatted stdin" test_checkStdin,
      testCase "inplace mode writes formatted files" test_inplace
    ]

test_stdinStdout :: IO ()
test_stdinStdout = do
  result <- runCLI missingReader missingWriter (Options ModeStdout [] []) "x=1\n"
  assertEqual "exit" ExitSuccess (cliExitCode result)
  assertEqual "stdout" "x =\n  1\n" (cliStdout result)
  assertEqual "stderr" "" (cliStderr result)

test_checkStdin :: IO ()
test_checkStdin = do
  result <- runCLI missingReader missingWriter (Options ModeCheck [] []) "x=1\n"
  assertEqual "exit" (ExitFailure 1) (cliExitCode result)
  assertEqual "stdout" "" (cliStdout result)
  assertEqual "stderr" "<stdin>: needs formatting\n" (cliStderr result)

test_inplace :: IO ()
test_inplace = do
  ref <- newIORef (M.singleton "Example.hs" "x=1\n")
  let reader path = do
        files <- readIORef ref
        pure (files M.! path)
      writer path contents = modifyIORef' ref (M.insert path contents)
  result <- runCLI reader writer (Options ModeInplace [] ["Example.hs"]) ""
  files <- readIORef ref
  assertEqual "exit" ExitSuccess (cliExitCode result)
  assertEqual "file" (Just "x =\n  1\n") (M.lookup "Example.hs" files)

missingReader :: FilePath -> IO Text
missingReader path = fail ("unexpected read: " <> path)

missingWriter :: FilePath -> Text -> IO ()
missingWriter path _ = fail ("unexpected write: " <> path)
