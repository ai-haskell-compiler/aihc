import System.Environment
import Control.Monad

import System.Linux.Btrfs

main :: IO ()
main = do
    paths <- getArgs
    forM_ paths $ \path -> do
        subvols <- listSubvolPaths path
        putStrLn (path ++ ":")
        forM_ subvols $ \(subvolId, parentId, subvolPath) ->
            putStrLn ("\t" ++ show subvolId ++
                      "\t" ++ show parentId ++
                      "\t" ++ subvolPath)
