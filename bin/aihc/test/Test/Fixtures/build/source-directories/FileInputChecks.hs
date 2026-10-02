module FileInputChecks (fileInputChecks) where

import Control.Monad (replicateM)
import System.Environment (getExecutablePath)
import System.IO
  ( IOMode (ReadMode),
    hGetChar,
    hGetLine,
    hIsEOF,
    readFile',
    withFile,
  )

fileInputChecks :: IO Bool
fileInputChecks = do
  executable <- getExecutablePath
  let path = executable ++ ".file-input"
  sizes <- mapM (checkSize path) [0, 8191, 8192, 8193, 16385]
  if and sizes then pure () else error "file input size check failed"
  let text = replicate 8191 'x' ++ "λ界🙂\n" ++ replicate 8193 'y'
  writeFile path text
  lazy <- readFile path
  strict <- readFile' path
  line <- withFile path ReadMode hGetLine
  characters <- withFile path ReadMode $ \handle -> do
    result <- replicateM (length text) (hGetChar handle)
    end <- hIsEOF handle
    pure (result == text && end)
  pure (lazy == text && strict == text && line == replicate 8191 'x' ++ "λ界🙂" && characters)

checkSize :: FilePath -> Int -> IO Bool
checkSize path size = do
  let text = replicate size 'x'
  writeFile path text
  lazy <- readFile path
  strict <- readFile' path
  if lazy == text && strict == text then pure True else pure False
