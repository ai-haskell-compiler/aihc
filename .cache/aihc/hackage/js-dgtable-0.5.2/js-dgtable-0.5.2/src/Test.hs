
module Main(main) where

import Paths_js_dgtable
import qualified Language.Javascript.DGTable as DGTable
import Data.Version


main :: IO ()
main = do
    length (versionBranch DGTable.version) === 3
    show DGTable.version === show version{versionBranch = take 3 $ versionBranch version}
    b <- readFile =<< DGTable.file
    min 1 (length b) === 1
    putStrLn "\nSuccess"


a === b | a == b = putChar '.'
        | otherwise = error $ show (a,b)
