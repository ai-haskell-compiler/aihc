{-|
Some wrappers for git commands.
-}

module SimpleCmd.Git (
  git,
  git_,
  gitBool,
  gitBranch,
  gitDiffQuiet,
  grepGitConfig,
  isGitDir,
  rwGitDir) where

import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import SimpleCmd (cmd, cmd_, cmdBool, egrep_)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif

-- | @git c args@ runs git command and return output
git :: String -- ^ git command
    -> [String] -- ^ arguments
    -> IO String -- ^ output
git c args =
  cmd "git" (c:args)

-- | @git_ c args@ run git command with output to stdout and stderr
git_ :: String -> [String] -> IO ()
git_ c args =
  cmd_ "git" (c:args)

-- | @gitBool c args@ runs git command and return result
--
-- @since 0.2.2
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args =
  cmdBool "git" (c:args)

-- | @isGitDir dir@ checks if directory has a .git/ subdir
isGitDir :: FilePath -> IO Bool
isGitDir dir = doesDirectoryExist (dir </> ".git")

-- | @gitBranch@ returns the git branch of the current directory
gitBranch :: IO String
gitBranch =
  git "rev-parse" ["--abbrev-ref", "HEAD"]

-- | @rwGitDir@ checks if a git repo is under ssh
rwGitDir :: IO Bool
rwGitDir =
  grepGitConfig "url = \\(ssh://\\|git@\\)"

-- | @grepGitConfig pat@ greps ".git/config" for extended regexp
--
-- @since 0.1.1
grepGitConfig :: String -> IO Bool
grepGitConfig key = do
  gitdir <- isGitDir "."
  if gitdir
    then egrep_ key ".git/config"
    else return False

-- | @gitDiffQuiet@ checks if unchanged
--
-- @since 0.1.3
gitDiffQuiet :: [String] -> IO Bool
gitDiffQuiet args = cmdBool "git" $ ["diff", "--quiet"] ++ args
