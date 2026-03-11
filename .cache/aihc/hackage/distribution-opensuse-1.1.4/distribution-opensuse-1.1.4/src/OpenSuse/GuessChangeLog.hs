{-# LANGUAGE OverloadedStrings #-}

module OpenSuse.GuessChangeLog ( guessChangeLog, GuessedChangeLog(..) ) where

import OpenSuse.StripSpace

import qualified Control.Foldl as Fold
import Control.Monad.Except
import Data.Algorithm.Diff
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Prelude hiding ( FilePath )
import Turtle hiding ( l, x, stderr, stdout )

-- | Automatically guess the differences between to releases of a package by
-- looking at the change log file provided by upstream. The function as
-- arguments the paths of two directories that contain the extracted release
-- tarballs. The first arguments ought to point to the older release, the
-- second paths ought to point to the updated version.
--
-- The function uses the following algorithm to detect the relevant changes:
--
--   1. Scan both directories for files that look like they might be change
--      logs.
--
--   2. If both directories contain the same candidate file, e.g. @ChangeLog@,
--      then use that.
--
--   3. Compute the differences between the change log files and check that all
--      modifications are additions at the top of the file.
--
--   4. Return those additions as 'Text'.

guessChangeLog :: FilePath -> FilePath -> IO GuessedChangeLog
guessChangeLog oldDir = fmap (either id id) . guessChangeLog' oldDir

guessChangeLog' :: FilePath -> FilePath -> IO (Either GuessedChangeLog GuessedChangeLog)
guessChangeLog' oldDir newDir = runExceptT $ do
  oldCLF <- Set.fromList <$> listShell (findChangeLogFiles oldDir)
  newCLF <- Set.fromList <$> listShell (findChangeLogFiles newDir)
  when (all null [oldCLF,newCLF]) (throwError NoChangeLogFiles)
  let clf' = oldCLF `Set.intersection` newCLF
  clf <- case Set.toAscList clf' of
           []    -> throwError (NoCommonChangeLogFiles oldCLF newCLF)
           [clf] -> return clf
           _     -> throwError (MoreThanOneChangeLogFile clf')
  old <- stripSpace <$> liftIO (Text.readFile (oldDir </> clf))
  new <- stripSpace <$> liftIO (Text.readFile (newDir </> clf))
  let changes    = cleanupEmptyLines (getDiff (Text.lines old) (Text.lines new))
      (top,diff) = span inBoth changes
      (add,foot) = span inSecond diff
      topAddOnly = all inBoth foot
  when (all inBoth changes) (throwError (UndocumentedUpdate clf))
  unless (length top < 10) (throwError (UnmodifiedTopIsTooLarge clf (fromIntegral (length top))))
  unless topAddOnly (throwError (NotJustTopAdditions clf))
  return (GuessedChangeLog clf (stripSpace (Text.unlines (map unDiff add))))

--

data GuessedChangeLog
    = GuessedChangeLog FilePath Text
        -- ^ Both releases contained the given change log file, and these files
        -- differed so that the given text was added at the top of the new one.
        -- The text undergoes some amount of cleanup, i.e. we'll trim leading
        -- empty lines at the top, trailing whitespace, and trailing empty
        -- lines at the end.
    | NoChangeLogFiles
        -- ^ Neither release contains a change log file.
    | UndocumentedUpdate FilePath
        -- ^ A change log file exists (and its name is returned), but it's
        -- identical in both releases. In other words, upstream probably forgot
        -- to document the release.
    | NoCommonChangeLogFiles (Set FilePath) (Set FilePath)
        -- ^ Both releases contain a set of files that look like they might be
        -- a change log, but their intersection is empty! This happens, for
        -- example, when upstream has renamed the file.
    | MoreThanOneChangeLogFile (Set FilePath)
        -- ^ Multiple change log files exists in both directories. Now, it
        -- would probably work out okay if we'd just look at the diffs of both
        -- of them, respectively, but it felt like a good idea to err on the
        -- side of caution. This case is rare anyways.
    | UnmodifiedTopIsTooLarge FilePath Word
        -- ^ 'guessChangelog' accepts up to 10 lines of unmodified text at the
        -- top of the upstream change log file because some people like to have
        -- a short introduction text there etc. If that header becomes too
        -- large, however, then we return this error because we expect upstream
        -- to add text at the top, not in the middle of the file.
    | NotJustTopAdditions FilePath
        -- ^ This happens when upstream edits the file in ways other than just
        -- adding at the top. Sometimes people re-format old entries or rewrite
        -- URLs or fix typos, and in such a case it feels to risky to trust the
        -- diff.
  deriving (Show)

cleanupEmptyLines :: [Diff Text] -> [Diff Text]
cleanupEmptyLines []                                        = []
cleanupEmptyLines (Second t1 : Both "" "" : Second t2 : xs) = Second t1 : Second "" : Second t2 : cleanupEmptyLines xs
cleanupEmptyLines (First  t1 : Both "" "" : First  t2 : xs) = First  t1 : First  "" : First  t2 : cleanupEmptyLines xs
cleanupEmptyLines (x:xs)                                    = x : cleanupEmptyLines xs

inBoth :: Diff a -> Bool
inBoth (Both _ _)   = True
inBoth _            = False

inSecond :: Diff a -> Bool
inSecond (Second _) = True
inSecond _          = False

unDiff :: Diff a -> a
unDiff (First txt)  = txt
unDiff (Both txt _) = txt
unDiff (Second txt) = txt

-- | This function finds any file in the given directory path that looks like
-- it might be a change log, meaning its name contains the word "change" and
-- its suffix is not one that obviously designates source code.

findChangeLogFiles :: FilePath -> Shell FilePath
findChangeLogFiles dirPath =
  onFiles (grepText changelogFilePattern) (filename <$> ls dirPath)

changelogFilePattern :: Pattern Text
changelogFilePattern = star dot <> asciiCI "change" <* invert codeSuffixPattern

codeSuffixPattern :: Pattern Text
codeSuffixPattern = choice [suffix ".hs", suffix ".c", suffix ".cpp"]

-- * Utility Functions

listShell :: MonadIO io => Shell a -> io [a]
listShell = flip fold Fold.list
