import System.Linux.FileExtents
import System.Environment
import System.IO
import Control.Monad
import Control.Exception

main :: IO ()
main = do
    args <- getArgs
    case args of
        "-v" : paths ->
            forM_ paths $ handleIOExn . printExtents
        paths ->
            forM_ paths $ handleIOExn . printExtentCount

printExtents :: FilePath -> IO ()
printExtents path = do
    exts <- getExtents defReqFlags path Nothing
    putStrLn $ path ++ ":"
    forM_ exts $ \ext ->
        putStrLn $ "    " ++ show ext
    putStrLn $ "  " ++ show (length exts) ++ " extent(s)"

printExtentCount :: FilePath -> IO ()
printExtentCount path = do
    c <- getExtentCount defReqFlags path Nothing
    putStrLn $ show c ++ "\t" ++ path

handleIOExn :: IO () -> IO ()
handleIOExn =
    handle (\e -> hPrint stderr (e ::  IOException))
