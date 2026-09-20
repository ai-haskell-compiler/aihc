module Main where

import GHC.IO.Exception (IOErrorType (InappropriateType, InvalidArgument))
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hClose, openFile)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError, tryIOError)

main :: IO ()
main = do
  arguments <- getArgs
  let directory = case arguments of
        [] -> "."
        ["--read", argument] -> argument
        argument : _ -> argument
      path = directory ++ "/corpus.txt"
  case arguments of
    ["--read", _] -> pure ()
    _ -> writeFile path "hello corpus\n"
  contents <- readFile path
  putStrLn ("read " ++ show (length contents) ++ " bytes")
  checkError (directory ++ "/missing.tsv") isDoesNotExistError
  checkError "" isDoesNotExistError
  checkError (path ++ "/child") ((== InappropriateType) . ioeGetErrorType)
  checkError (directory ++ "/bad\0name") ((== InvalidArgument) . ioeGetErrorType)
  checkError (directory ++ "/bad" ++ [toEnum 55296] ++ "name") ((== InvalidArgument) . ioeGetErrorType)

checkError :: FilePath -> (IOError -> Bool) -> IO ()
checkError path predicate = do
  result <- tryIOError (openFile path ReadMode)
  case result of
    Left err -> print (predicate err)
    Right handle -> hClose handle >> print False
