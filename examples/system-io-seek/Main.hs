module Main (main) where

import System.IO
  ( IOMode (ReadMode, WriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek),
    hClose,
    hGetLine,
    hIsSeekable,
    hPutStr,
    hSeek,
    hTell,
    openFile,
    withFile,
  )

-- A file moves its position, and a read starts at the new position. The
-- Hackage index reader jumps to the entry that it wants in a large archive
-- in this way.
main :: IO ()
main = do
  withFile "seek.txt" WriteMode (\handle -> hPutStr handle "alpha\nbeta\ngamma\ndelta\n")
  handle <- openFile "seek.txt" ReadMode
  seekable <- hIsSeekable handle
  print seekable
  hSeek handle AbsoluteSeek 11
  hGetLine handle >>= putStrLn
  hTell handle >>= print
  hSeek handle RelativeSeek (-6)
  hGetLine handle >>= putStrLn
  hSeek handle AbsoluteSeek 0
  hGetLine handle >>= putStrLn
  hClose handle
