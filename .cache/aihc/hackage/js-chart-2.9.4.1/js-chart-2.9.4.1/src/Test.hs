
module Main(main) where

import Paths_js_chart
import qualified Language.Javascript.Chart as Chart
import Data.Version
import Control.Monad


main :: IO ()
main = do
    length (versionBranch Chart.version) === 3
    show Chart.version === show version{versionBranch = take 3 $ versionBranch version}
    forM_ [minBound..maxBound] $ \i -> do
        s <- readFile =<< Chart.file i
        min 1 (length s) === 1
    putStrLn "\nSuccess"


a === b | a == b = putChar '.'
        | otherwise = error $ show (a,b)
