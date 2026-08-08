module System.Exit
  ( ExitCode (..),
    exitWith,
    exitFailure,
    exitSuccess,
    die,
  )
where

import Control.Exception (Exception (..), throwIO)
import GHC.IO.Exception (ioError, userError)
import GHC.IO.Output (writeStderrString)
import GHC.Read ()
import Prelude

data ExitCode
  = ExitSuccess
  | ExitFailure Int

instance Eq ExitCode where
  ExitSuccess == ExitSuccess = True
  ExitFailure left == ExitFailure right = left == right
  _ == _ = False
  left /= right = not (left == right)

instance Ord ExitCode where
  compare ExitSuccess ExitSuccess = EQ
  compare ExitSuccess (ExitFailure _) = LT
  compare (ExitFailure _) ExitSuccess = GT
  compare (ExitFailure left) (ExitFailure right) = compare left right
  left < right = compare left right == LT
  left <= right = compare left right /= GT
  left > right = compare left right == GT
  left >= right = compare left right /= LT
  max left right =
    case compare left right of
      GT -> left
      _ -> right
  min left right =
    case compare left right of
      GT -> right
      _ -> left

instance Show ExitCode where
  showsPrec _ ExitSuccess = showString "ExitSuccess"
  showsPrec precedence (ExitFailure status) =
    showParen
      (precedence > 10)
      (showString "ExitFailure " . showsPrec 11 status)

instance Read ExitCode where
  readsPrec precedence input =
    readExitSuccess input
      ++ readParen (precedence > 10) readExitFailure input

readExitSuccess :: ReadS ExitCode
readExitSuccess input =
  case lex input of
    (token, rest) : _ ->
      case token == "ExitSuccess" of
        True -> [(ExitSuccess, rest)]
        False -> []
    _ -> []

readExitFailure :: ReadS ExitCode
readExitFailure input =
  case lex input of
    (token, rest) : _ ->
      case token == "ExitFailure" of
        True -> readExitFailureStatus (reads rest)
        False -> []
    _ -> []

readExitFailureStatus :: [(Int, String)] -> [(ExitCode, String)]
readExitFailureStatus [] = []
readExitFailureStatus ((status, rest) : results) =
  (ExitFailure status, rest) : readExitFailureStatus results

instance Exception ExitCode where
  displayException = show

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = throwIO ExitSuccess
exitWith code@(ExitFailure status) =
  case status == 0 of
    True -> ioError (userError "exitWith: invalid argument (ExitFailure 0)")
    False -> throwIO code

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

die :: String -> IO a
die message = do
  writeStderrString message
  exitFailure
