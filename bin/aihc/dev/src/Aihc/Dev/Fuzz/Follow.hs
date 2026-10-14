{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Follow the selected branch of a repository. Build each new commit in
-- the background, and start the new @aihc-dev@ when its build is ready.
--
-- The builds use two worktrees in the cache directory, in turn. The running
-- executable reads sources from one worktree, and the next build changes
-- only the other worktree. Environment variables give the repository, the
-- commit, and the worktree to the next executable.
module Aihc.Dev.Fuzz.Follow
  ( Follower,
    Relaunch (..),
    followLoop,
    relaunch,
    startFollower,
  )
where

import Aihc.Dev.Fuzz.Model (FollowStatus (..))
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.List (dropWhileEnd)
import Data.Map.Strict qualified as Map
import GHC.Environment (getFullArgs)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesDirectoryExist, getXdgDirectory, setCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hFlush, stderr, stdout, withFile)
import System.Posix.Process (executeFile)
import System.Process (CreateProcess (..), StdStream (..), proc, readCreateProcessWithExitCode, waitForProcess, withCreateProcess)

data Follower = Follower
  { -- | The top directory of the repository that has the selected branch.
    followerRepository :: FilePath,
    -- | The commit of the running executable.
    followerCommit :: String,
    -- | The worktree of the running executable. The first executable runs
    -- from a checkout of the user, so it has no worktree.
    followerSlot :: Maybe String
  }

-- | A new executable that is ready to run.
data Relaunch = Relaunch
  { relaunchExecutable :: FilePath,
    relaunchRoot :: FilePath,
    relaunchRepository :: FilePath,
    relaunchCommit :: String,
    relaunchSlot :: String
  }

repositoryVariable, commitVariable, slotVariable :: String
repositoryVariable = "AIHC_FUZZ_FOLLOW_REPOSITORY"
commitVariable = "AIHC_FUZZ_FOLLOW_COMMIT"
slotVariable = "AIHC_FUZZ_FOLLOW_SLOT"

-- | Look for new commits once each minute.
pollMicroseconds :: Int
pollMicroseconds = 60 * 1000000

-- | Find the repository and the commit of the running executable. An
-- executable that this module started gets them from the environment.
-- Otherwise, the repository contains the current directory, and the
-- executable has the commit at its @HEAD@.
startFollower :: IO (Either String Follower)
startFollower = do
  inheritedRepository <- lookupEnv repositoryVariable
  inheritedCommit <- lookupEnv commitVariable
  slot <- lookupEnv slotVariable
  repository <- maybe (git "." ["rev-parse", "--show-toplevel"]) (pure . Right) inheritedRepository
  case repository of
    Left reason -> pure (Left ("cannot find the repository to follow: " <> reason))
    Right root -> do
      commit <- maybe (git root ["rev-parse", "HEAD"]) (pure . Right) inheritedCommit
      pure $ case commit of
        Left reason -> Left ("cannot find the commit of the running executable: " <> reason)
        Right running ->
          Right Follower {followerRepository = root, followerCommit = running, followerSlot = slot}

-- | Look for a new commit on the selected branch until a build of one is
-- ready. A commit that does not build is not tried again.
followLoop :: Follower -> (FollowStatus -> IO ()) -> IO Relaunch
followLoop follower@Follower {followerRepository, followerCommit} report = go Nothing
  where
    pause = threadDelay pollMicroseconds
    go failed = do
      target <- selectTarget followerRepository
      case target of
        Left reason -> report (FollowError reason) >> pause >> go failed
        Right (branch, commit)
          | commit == followerCommit -> report (FollowWatching branch followerCommit) >> pause >> go failed
          | Just commit == failed -> pause >> go failed
          | otherwise -> do
              report (FollowBuilding branch commit)
              built <- buildCommit follower commit
              case built of
                Right ready -> pure ready
                Left reason -> report (FollowFailed branch commit reason) >> pause >> go (Just commit)

-- | Give the selected branch and the commit to build. If the branch has an
-- upstream branch that only adds commits, build the upstream commit.
selectTarget :: FilePath -> IO (Either String (String, String))
selectTarget repository = do
  symbolic <- git repository ["symbolic-ref", "--quiet", "--short", "HEAD"]
  let branch = fromRight "HEAD" symbolic
  local <- git repository ["rev-parse", "HEAD"]
  case local of
    Left reason -> pure (Left reason)
    Right localCommit -> do
      upstream <- git repository ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"]
      case upstream of
        Left _ -> pure (Right (branch, localCommit))
        Right upstreamName -> do
          -- Without a network, use the upstream commit that is known.
          _ <- git repository ["fetch", "--quiet"]
          upstreamCommit <- git repository ["rev-parse", upstreamName]
          ahead <- gitSucceeds repository ["merge-base", "--is-ancestor", localCommit, fromRight localCommit upstreamCommit]
          pure $ Right $ case upstreamCommit of
            Right commit | ahead -> (branch, commit)
            _ -> (branch, localCommit)

-- | Check out the commit in the worktree that does not run, and build
-- @aihc-dev@ there. The build output goes to a log file.
buildCommit :: Follower -> String -> IO (Either String Relaunch)
buildCommit Follower {followerRepository, followerSlot} commit = do
  cache <- getXdgDirectory XdgCache ("aihc" </> "fuzz")
  let slot = if followerSlot == Just "a" then "b" else "a"
      root = cache </> "worktrees" </> slot
      logFile = cache </> ("build-" <> slot <> ".log")
  createDirectoryIfMissing True (cache </> "worktrees")
  exists <- doesDirectoryExist root
  checkout <-
    if exists
      then git root ["checkout", "--quiet", "--force", "--detach", commit]
      else do
        _ <- git followerRepository ["worktree", "prune"]
        git followerRepository ["worktree", "add", "--quiet", "--force", "--detach", root, commit]
  case checkout of
    Left reason -> pure (Left ("cannot check out the commit: " <> reason))
    Right _ -> do
      built <- withFile logFile WriteMode $ \logHandle ->
        tryIO $
          withCreateProcess
            (proc "cabal" ["build", "exe:aihc-dev"]) {cwd = Just root, std_in = NoStream, std_out = UseHandle logHandle, std_err = UseHandle logHandle}
            (\_ _ _ process -> waitForProcess process)
      case built of
        Right ExitSuccess -> do
          executable <- run root "cabal" ["list-bin", "-v0", "exe:aihc-dev"]
          pure $ case executable of
            Left reason -> Left ("cannot find the new executable: " <> reason)
            Right path ->
              Right
                Relaunch
                  { relaunchExecutable = path,
                    relaunchRoot = root,
                    relaunchRepository = followerRepository,
                    relaunchCommit = commit,
                    relaunchSlot = slot
                  }
        Right (ExitFailure _) -> pure (Left ("the build failed, see " <> logFile))
        Left reason -> pure (Left ("cannot start cabal: " <> reason))

-- | Replace this process with the new executable. It gets the same
-- arguments, and it reads its sources from its worktree.
relaunch :: Relaunch -> IO a
relaunch Relaunch {relaunchExecutable, relaunchRoot, relaunchRepository, relaunchCommit, relaunchSlot} = do
  -- The full arguments start with the program name and keep the RTS options.
  arguments <- drop 1 <$> getFullArgs
  environment <- getEnvironment
  let updated =
        Map.toList $
          Map.union
            ( Map.fromList
                [ (repositoryVariable, relaunchRepository),
                  (commitVariable, relaunchCommit),
                  (slotVariable, relaunchSlot),
                  ("AIHC_TEST_ROOT", relaunchRoot)
                ]
            )
            (Map.fromList environment)
  putStrLn ("Starting aihc-dev from commit " <> take 12 relaunchCommit <> ".")
  hFlush stdout
  hFlush stderr
  setCurrentDirectory relaunchRoot
  executeFile relaunchExecutable False arguments (Just updated)

git :: FilePath -> [String] -> IO (Either String String)
git directory = run directory "git"

gitSucceeds :: FilePath -> [String] -> IO Bool
gitSucceeds directory arguments =
  either (const False) (const True) <$> git directory arguments

-- | Run a command and give its output without the surrounding space.
run :: FilePath -> String -> [String] -> IO (Either String String)
run directory command arguments = do
  result <- tryIO (readCreateProcessWithExitCode (proc command arguments) {cwd = Just directory} "")
  pure $ case result of
    Left reason -> Left reason
    Right (ExitSuccess, out, _) -> Right (trim out)
    Right (ExitFailure code, out, err) ->
      Left (command <> " exited with " <> show code <> ": " <> firstLine (err <> out))
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
    firstLine = takeWhile (/= '\n') . trim

tryIO :: IO a -> IO (Either String a)
tryIO action = do
  result <- try action
  pure $ case result of
    Left (exception :: IOException) -> Left (show exception)
    Right value -> Right value
