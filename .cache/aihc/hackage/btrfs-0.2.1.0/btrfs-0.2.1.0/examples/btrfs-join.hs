import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import Control.Exception (bracket)
import System.Posix

import System.Linux.Btrfs (cloneRangeFd)

main :: IO ()
main = do
    args <- getArgs
    case args of
        dstPath : srcFiles0@(_ : _) ->
            withWriteFd dstPath $ \dstFd -> do
                let loop offset (srcFile : srcFiles) = do
                        len <- withReadFd srcFile $ \srcFd -> do
                            cloneRangeFd srcFd 0 0 dstFd offset
                            fileSize <$> getFdStatus srcFd
                        loop (offset + fromIntegral len) srcFiles
                    loop _ [] = return ()
                loop 0 srcFiles0
        _ -> do
            prog <- getProgName
            hPutStrLn stderr "Invalid command line arguments"
            hPutStrLn stderr $ "Usage: " ++ prog ++ " DEST SOURCE [SOURCE]..."
            exitFailure

withReadFd :: FilePath -> (Fd -> IO r) -> IO r
withReadFd path action =
    bracket
        (openFd path ReadOnly Nothing defaultFileFlags {nonBlock = True})
        closeFd action

withWriteFd :: FilePath -> (Fd -> IO r) -> IO r
withWriteFd path action =
    bracket
        (openFd path WriteOnly (Just stdFileMode) defaultFileFlags {trunc = True})
        closeFd action
