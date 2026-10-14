{-# LANGUAGE ScopedTypeVariables #-}

-- | Report known bugs as GitHub issues with the @gh@ command.
module Aihc.Dev.Fuzz.Report
  ( GhStatus (..),
    probeGh,
    reporterLoop,
  )
where

import Aihc.Dev.Fuzz.Bugs (Bug (..), IssueRef (..), commentBody, issueBody, issueTitle, parseIssueRef)
import Aihc.Dev.Fuzz.Model (ReporterStatus (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, displayException, try)
import Control.Monad (forever, void, when)
import Data.Char (isSpace)
import Data.Maybe (isNothing, listToMaybe)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

data GhStatus
  = GhReady
  | GhMissing
  | GhUnauthorized String
  deriving (Eq, Show)

-- | Find out whether @gh@ is installed and logged in to github.com.
probeGh :: IO GhStatus
probeGh = do
  executable <- findExecutable "gh"
  case executable of
    Nothing -> pure GhMissing
    Just _ -> do
      result <- try (readProcessWithExitCode "gh" ["auth", "status", "--hostname", "github.com"] "")
      pure $ case result of
        Left (exception :: IOException) -> GhUnauthorized (displayException exception)
        Right (ExitSuccess, _, _) -> GhReady
        Right (ExitFailure _, out, err) -> GhUnauthorized (firstLine (err <> out))

runGh :: [String] -> String -> IO (Either String String)
runGh arguments input = do
  result <- try (readProcessWithExitCode "gh" arguments input)
  pure $ case result of
    Left (exception :: IOException) -> Left (displayException exception)
    Right (ExitSuccess, out, _) -> Right out
    Right (ExitFailure code, out, err) -> Left ("gh exited with " <> show code <> ": " <> firstLine (err <> out))

createIssue :: String -> Bug -> IO (Either String IssueRef)
createIssue repository bug = do
  result <- runGh ["issue", "create", "--repo", repository, "--title", issueTitle bug, "--label", "bug", "--body-file", "-"] (issueBody bug)
  pure $ do
    output <- result
    maybe (Left ("cannot read the issue URL from: " <> firstLine output)) Right (parseIssueRef output)

commentIssue :: String -> IssueRef -> Bug -> IO (Either String ())
commentIssue repository issue bug =
  void <$> runGh ["issue", "comment", show (issueNumber issue), "--repo", repository, "--body-file", "-"] (commentBody bug)

-- | Report each bug without an issue, oldest first, until the thread is cancelled.
-- The last callback runs on each change of the reporter status.
-- The first bug of a property opens an issue. A later bug of the same
-- property becomes a comment on that issue. When @gh@ is not usable, the
-- loop tries again later.
reporterLoop :: String -> TVar [Bug] -> TVar ReporterStatus -> IO () -> (Bug -> IO ()) -> (ReporterStatus -> IO ()) -> IO ()
reporterLoop repository bugsVar statusVar persist announce announceStatus = forever $ do
  pending <- filter (isNothing . bugIssue) <$> readTVarIO bugsVar
  if null pending
    then setStatus ReporterIdle >> threadDelay pollMicroseconds
    else do
      gh <- probeGh
      case gh of
        GhMissing -> setStatus (ReporterBlocked "gh is not installed") >> threadDelay retryMicroseconds
        GhUnauthorized reason -> setStatus (ReporterBlocked ("gh is not authorized: " <> reason)) >> threadDelay retryMicroseconds
        GhReady -> do
          done <- reportAll pending
          if done then setStatus ReporterIdle else threadDelay retryMicroseconds
  where
    pollMicroseconds = 2000000
    retryMicroseconds = 15000000
    setStatus status = do
      changed <- atomically $ do
        current <- readTVar statusVar
        when (current /= status) (writeTVar statusVar status)
        pure (current /= status)
      when changed (announceStatus status)
    reportAll [] = pure True
    reportAll (bug : rest) = do
      setStatus (ReporterSending (bugId bug))
      bugs <- readTVarIO bugsVar
      let existing = listToMaybe [issue | other <- bugs, bugProperty other == bugProperty bug, Just issue <- [bugIssue other]]
      result <- case existing of
        Just issue -> fmap (const issue) <$> commentIssue repository issue bug
        Nothing -> createIssue repository bug
      case result of
        Left reason -> setStatus (ReporterFailed reason) >> pure False
        Right issue -> do
          atomically $ modifyTVar' bugsVar (map (\other -> if bugId other == bugId bug then other {bugIssue = Just issue} else other))
          persist
          announce bug {bugIssue = Just issue}
          reportAll rest

-- | The first line that says something: @gh@ marks its errors with @X@.
firstLine :: String -> String
firstLine output =
  case filter (not . null) (map strip (lines output)) of
    [] -> ""
    stripped@(first : _) ->
      case [drop 2 line | line <- stripped, take 2 line == "X "] of
        marked : _ -> marked
        [] -> first
  where
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
