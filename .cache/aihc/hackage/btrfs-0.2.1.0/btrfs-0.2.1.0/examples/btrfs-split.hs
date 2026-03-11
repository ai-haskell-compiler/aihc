import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import Text.Read (readMaybe)
import Text.Printf (printf)
import Control.Exception (bracket)
import Data.Function (fix)
import Control.Monad (when)
import System.Posix

import System.Linux.Btrfs (cloneRangeFd)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [srcPath, sizeS, prefix] | Just size <- readMaybe sizeS, size > 0 ->
            withReadFd srcPath $ \srcFd -> do
                stat <- getFdStatus srcFd
                let totSize = fromIntegral $ fileSize stat
                    mode = fileMode stat
                    lastN = (totSize - 1) `div` size
                    formatStr = "%0" ++ show (length (show lastN)) ++ "d"
                flip fix (0 :: Int, 0) $ \loop (n, offset) -> do
                    let filename = prefix ++ printf formatStr n
                        size' = min size (totSize - offset)
                    withWriteFd filename mode $ \dstFd ->
                        cloneRangeFd srcFd offset size' dstFd 0
                    let offset' = offset + size'
                    when (offset' < totSize) $ loop (n + 1, offset')
        _ -> do
            prog <- getProgName
            hPutStrLn stderr "Invalid command line arguments"
            hPutStrLn stderr $ "Usage: " ++ prog ++ " FILE SIZE PREFIX"
            exitFailure

withReadFd :: FilePath -> (Fd -> IO r) -> IO r
withReadFd path action =
    bracket
        (openFd path ReadOnly Nothing defaultFileFlags {nonBlock = True})
        closeFd action

withWriteFd :: FilePath -> FileMode -> (Fd -> IO r) -> IO r
withWriteFd path mode action =
    bracket
        (openFd path WriteOnly (Just mode) defaultFileFlags {trunc = True})
        closeFd action
