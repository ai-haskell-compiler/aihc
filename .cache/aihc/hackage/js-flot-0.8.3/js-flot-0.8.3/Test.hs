
module Main(main) where

import Paths_js_flot
import qualified Language.Javascript.Flot as Flot
import Data.Version
import Control.Monad


main :: IO ()
main = do
    length (versionBranch Flot.version) === 3
    versionTags Flot.version === []
    versionBranch Flot.version === take 3 (versionBranch version)
    forM_ [minBound..maxBound] $ \i -> do
        s <- readFile =<< Flot.file i
        min 1 (length s) === 1
    putStrLn "\nSuccess"


a === b | a == b = putChar '.'
        | otherwise = error $ show (a,b)
