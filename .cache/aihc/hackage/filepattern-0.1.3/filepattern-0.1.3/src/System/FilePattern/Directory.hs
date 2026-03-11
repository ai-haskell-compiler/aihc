
-- | Optimised directory traversal using 'FilePattern' values.
--   All results are guaranteed to be sorted.
--
--   /Case Sensitivity/: these traversals are optimised to reduce the number of IO operations
--   performed. In particular, if the relevant subdirectories can be determined in
--   advance it will use 'doesDirectoryExist' rather than 'getDirectoryContents'.
--   However, on case-insensitive file systems, if there is a directory @foo@,
--   then @doesDirectoryExist \"FOO\"@ will report @True@, but @FOO@ won't be a result
--   returned by 'getDirectoryContents', which may result in different search results
--   depending on whether a certain optimisations kick in.
--
--   If these optimisation differences are absolutely unacceptable use 'getDirectoryFilesIgnoreSlow'.
--   However, normally these differences are not a problem.
module System.FilePattern.Directory(
    FilePattern,
    getDirectoryFiles,
    getDirectoryFilesIgnore,
    getDirectoryFilesIgnoreSlow
    ) where

import Control.Monad.Extra
import Data.Functor
import Data.List
import System.Directory
import System.FilePath
import System.FilePattern.Core
import System.FilePattern.Step
import Prelude


-- | Get the files below a certain root that match any of the 'FilePattern' values. Only matches
--   files, not directories. Avoids traversing into directories that it can detect won't have
--   any matches in.
--
-- > getDirectoryFiles "myproject/src" ["**/*.h","**/*.c"]
--
--   If there are certain directories/files that should not be explored, use 'getDirectoryFilesIgnore'.
--
--   /Warning/: on case-insensitive file systems certain optimisations can cause surprising results.
--   See the top of the module for details.
getDirectoryFiles :: FilePath -> [FilePattern] -> IO [FilePath]
getDirectoryFiles dir match = operation False dir match []



-- | Get the files below a certain root matching any of the first set of 'FilePattern' values,
--   but don't return any files which match any ignore pattern (the final argument).
--   Typically the ignore pattens will end with @\/**@, e.g. @.git\/**@.
--
-- > getDirectoryFilesIgnore "myproject/src" ["**/*.h","**/*.c"] [".git/**"]
--
--   /Warning/: on case-insensitive file systems certain optimisations can cause surprising results.
--   See the top of the module for details.
getDirectoryFilesIgnore :: FilePath -> [FilePattern] -> [FilePattern] -> IO [FilePath]
getDirectoryFilesIgnore = operation False


-- | Like 'getDirectoryFilesIgnore' but that the optimisations that may change behaviour on a
--   case-insensitive file system. Note that this function will never return more results
--   then 'getDirectoryFilesIgnore', and may return less. However, it will obey invariants
--   such as:
--
-- > getDirectoryFilesIgnoreSlow root [x] [] ++ getDirectoryFilesIgnoreSlow root [y] []
-- >     == getDirectoryFilesIgnoreSlow root [x,y] []
--
--   In contrast 'getDirectoryFilesIgnore' only guarantees that invariant on
--   case-sensitive file systems.
getDirectoryFilesIgnoreSlow :: FilePath -> [FilePattern] -> [FilePattern] -> IO [FilePath]
getDirectoryFilesIgnoreSlow = operation True


operation :: Bool -> FilePath -> [FilePattern] -> [FilePattern] -> IO [FilePath]
operation slow rootBad yes no = f [] (step_ yes) (step_ no)
    where
        -- normalise out Windows vs other behaviour around "", make sure we end with /
        root = if rootBad == "" then "./" else addTrailingPathSeparator rootBad

        -- parts is a series of path components joined with trailing / characters
        f parts yes no
            | StepEverything <- stepNext no = pure []
            | not slow, StepOnly xs <- stepNext yes = g parts yes no xs
            | otherwise = do
                xs <- filter (not . all (== '.')) <$> getDirectoryContents (root ++ parts)
                g parts yes no xs

        g parts yes no xs =
            concatForM (sort xs) $ \x -> do
                let path = root ++ parts ++ x
                -- deliberately shadow since using yes/no from now on would be wrong
                yes <- pure $ stepApply yes x
                no <- pure $ stepApply no x
                isFile <- whenMaybe (stepDone yes /= [] && stepDone no == []) (doesFileExist path)
                case isFile of
                    Just True -> pure [parts ++ x]
                    _ | StepEverything <- stepNext no -> pure []
                      | StepOnly [] <- stepNext yes -> pure []
                      | otherwise -> do
                        -- Here we used to assume that getDirectoryContents means something exists,
                        -- doesFileExists is False, therefore this must be a directory.
                        -- That's not true in the presence of symlinks.
                        b <- doesDirectoryExist path
                        if not b then pure [] else f (parts ++ x ++ "/") yes no
