{-# LANGUAGE ForeignFunctionInterface #-}

-- A Handle over a descriptor of the libc. openBinaryTempFile opens the file
-- with a libc call and wraps the descriptor, so writing, closing, and reading
-- the file back go through the runtime and the libc together.
module Main where

import Data.List (isInfixOf)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import System.IO

foreign import ccall unsafe "unlink" c_unlink :: CString -> IO CInt

main :: IO ()
main = do
  (path, handle) <- openBinaryTempFile "." "aihc-example.txt"
  putStrLn ("name keeps the template: " ++ show ("aihc-example" `isInfixOf` path))
  hPutStr handle "first line\nsecond line\n"
  hClose handle
  contents <- readFile path
  putStr contents
  appendHandle <- openFile path AppendMode
  hPutStrLn appendHandle "third line"
  hClose appendHandle
  readBack <- readFile path
  putStrLn ("lines after append: " ++ show (length (lines readBack)))
  removed <- withCString path c_unlink
  putStrLn ("unlink: " ++ show removed)
  hFlush stdout
