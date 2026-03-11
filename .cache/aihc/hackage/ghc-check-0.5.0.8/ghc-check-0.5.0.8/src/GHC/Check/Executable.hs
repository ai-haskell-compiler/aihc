{- | Discover the GHC version by querying the GHC executable

 -}
module GHC.Check.Executable where

import Data.Version
import System.FilePath
import System.Process
import Text.ParserCombinators.ReadP
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty


-- | Takes a path to the GHC binary to query.
--   Throws if anything goes wrong.
getGhcVersion :: FilePath -> IO Version
getGhcVersion fp = do
    out <- readProcess fp ["--numeric-version"] ""
    case readP_to_S (parseVersion <* eof) (trim out) of
        [(v, "")] -> return v
        _ -> error $ "Failed to parse GHC version: " <> out

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Returns a list of possible paths for the GHC executable
guessExecutablePathFromLibdir :: FilePath -> NonEmpty FilePath
guessExecutablePathFromLibdir fp = NonEmpty.fromList
    [ fp </> "bin" </> "ghc"               -- Linux
    , fp </> ".." </> "bin" </> "ghc"      -- Linux (Relocatable GHC build)
    , fp </> ".." </> "bin" </> "ghc.exe"  -- Windows
    ]
