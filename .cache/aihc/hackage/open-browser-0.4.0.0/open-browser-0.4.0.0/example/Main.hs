{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Control.Exception ( Exception (..), SomeException )
import System.Exit ( ExitCode (..) )
import Web.Browser ( openBrowser, openBrowserWithExitCode )

main :: IO ()
main = do
  openBrowser "https://haskell.org/" >>= \case
    True -> putStrLn "The operation succeeded!"
    False -> putStrLn "The operation failed!"
  blankLine
  putStrLn "Help with debugging:"
  putStrLn "Try a good URL:"
  tryOpenGoodUrl >>= reportResult
  blankLine
  putStrLn "Try a bad URL:"
  tryOpenBadUrl >>= reportResult
 where
  tryOpenGoodUrl :: IO (Either SomeException (ExitCode, String, String))
  tryOpenGoodUrl = openBrowserWithExitCode "https://haskell.org/"
  tryOpenBadUrl :: IO (Either SomeException (ExitCode, String, String))
  tryOpenBadUrl = openBrowserWithExitCode "example-bad-url"
  reportResult = \case
    Left e -> putStrLn $ "Exception: " <> displayException e
    Right (ec, out, err) -> do
      putStrLn $ "Exit code: " <> show ec
      putStrLn $ "Standard output: " <> out
      putStrLn $ "Standard error: " <> err
  blankLine = putStrLn ""
