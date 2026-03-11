import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import Text.Read (readMaybe)

import System.Linux.Btrfs

main :: IO ()
main = do
    args <- getArgs
    case args of
        [srcPath, srcOffS, srcLenS, dstPath, dstOffS]
            | Just srcOff <- readMaybe srcOffS
            , Just srcLen <- readMaybe srcLenS
            , Just dstOff <- readMaybe dstOffS ->
                cloneRange srcPath srcOff srcLen dstPath dstOff
        _ -> do
            prog <- getProgName
            hPutStrLn stderr "Invalid command line arguments"
            hPutStrLn stderr $
                "Usage: " ++ prog ++
                " SOURCE SOURCE_OFF SOURCE_LEN DEST DEST_OFF"
            exitFailure
