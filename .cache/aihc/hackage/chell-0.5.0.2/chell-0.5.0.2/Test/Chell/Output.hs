{-# LANGUAGE CPP #-}

module Test.Chell.Output
  ( Output,
    outputStart,
    outputResult,
    ColorMode (..),
    plainOutput,
    colorOutput,
  )
where

import Control.Monad (forM_, unless, when)

#ifdef MIN_VERSION_ansi_terminal
import qualified System.Console.ANSI as AnsiTerminal
#endif

import Test.Chell.Types

data Output = Output
  { outputStart :: Test -> IO (),
    outputResult :: Test -> TestResult -> IO ()
  }

plainOutput :: Bool -> Output
plainOutput v =
  Output
    { outputStart = plainOutputStart v,
      outputResult = plainOutputResult v
    }

plainOutputStart :: Bool -> Test -> IO ()
plainOutputStart v t =
  when v $
    do
      putStr "[ RUN   ] "
      putStrLn (testName t)

plainOutputResult :: Bool -> Test -> TestResult -> IO ()
plainOutputResult v t (TestPassed _) =
  when v $
    do
      putStr "[ PASS  ] "
      putStrLn (testName t)
      putStrLn ""
plainOutputResult v t TestSkipped =
  when v $
    do
      putStr "[ SKIP  ] "
      putStrLn (testName t)
      putStrLn ""
plainOutputResult _ t (TestFailed notes fs) =
  do
    putStr "[ FAIL  ] "
    putStrLn (testName t)
    printNotes notes
    printFailures fs
plainOutputResult _ t (TestAborted notes msg) =
  do
    putStr "[ ABORT ] "
    putStrLn (testName t)
    printNotes notes
    putStr "  "
    putStr msg
    putStrLn "\n"
plainOutputResult _ _ _ = return ()

data ColorMode
  = ColorModeAuto
  | ColorModeAlways
  | ColorModeNever
  deriving (Enum)

colorOutput :: Bool -> Output
#ifndef MIN_VERSION_ansi_terminal
colorOutput = plainOutput
#else
colorOutput v =
  Output
    { outputStart = colorOutputStart v
    , outputResult = colorOutputResult v
    }

colorOutputStart :: Bool -> Test -> IO ()
colorOutputStart v t = when v $
  do
    putStr "[ RUN   ] "
    putStrLn (testName t)

colorOutputResult :: Bool -> Test -> TestResult -> IO ()
colorOutputResult v t (TestPassed _) =
    when v $
      do
        putStr "[ "
        AnsiTerminal.setSGR
            [ AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid AnsiTerminal.Green
            ]
        putStr "PASS"
        AnsiTerminal.setSGR
            [ AnsiTerminal.Reset
            ]
        putStr "  ] "
        putStrLn (testName t)
        putStrLn ""
colorOutputResult v t TestSkipped =
    when v $
      do
        putStr "[ "
        AnsiTerminal.setSGR
            [ AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid AnsiTerminal.Yellow
            ]
        putStr "SKIP"
        AnsiTerminal.setSGR
            [ AnsiTerminal.Reset
            ]
        putStr "  ] "
        putStrLn (testName t)
        putStrLn ""
colorOutputResult _ t (TestFailed notes fs) =
  do
    putStr "[ "
    AnsiTerminal.setSGR
        [ AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid AnsiTerminal.Red
        ]
    putStr "FAIL"
    AnsiTerminal.setSGR
        [ AnsiTerminal.Reset
        ]
    putStr "  ] "
    putStrLn (testName t)
    printNotes notes
    printFailures fs
colorOutputResult _ t (TestAborted notes msg) =
  do
    putStr "[ "
    AnsiTerminal.setSGR
        [ AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid AnsiTerminal.Red
        ]
    putStr "ABORT"
    AnsiTerminal.setSGR
        [ AnsiTerminal.Reset
        ]
    putStr " ] "
    putStrLn (testName t)
    printNotes notes
    putStr "  "
    putStr msg
    putStrLn "\n"
colorOutputResult _ _ _ = return ()
#endif

printNotes :: [(String, String)] -> IO ()
printNotes notes =
  unless (null notes) $
    do
      forM_ notes $ \(key, value) ->
        do
          putStr "  note: "
          putStr key
          putStr "="
          putStrLn value
      putStrLn ""

printFailures :: [Failure] -> IO ()
printFailures fs =
  forM_ fs $ \f ->
    do
      putStr "  "
      case failureLocation f of
        Just loc ->
          do
            putStr (locationFile loc)
            putStr ":"
            case locationLine loc of
              Just line -> putStrLn (show line)
              Nothing -> putStrLn ""
        Nothing -> return ()
      putStr "  "
      putStr (failureMessage f)
      putStrLn "\n"
