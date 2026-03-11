{-# LANGUAGE MultiParamTypeClasses
            ,FunctionalDependencies #-}


{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike.IO
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD3

   Maintainer : David Fox <dsf@seereason.com>, Andreas Abel
   Stability  : stable
   Portability: portable

String-like functions

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.ListLike.IO
    ( ListLikeIO(..)
    )
       where
import Prelude hiding (length, head, last, null, tail, map, filter, concat,
                       any, lookup, init, all, foldl, foldr, foldl1, foldr1,
                       maximum, minimum, iterate, span, break, takeWhile,
                       dropWhile, reverse, zip, zipWith, sequence,
                       sequence_, mapM, mapM_, concatMap, and, or, sum,
                       product, repeat, replicate, cycle, take, drop,
                       splitAt, elem, notElem, unzip, lines, words,
                       unlines, unwords, putStr, getContents)
import qualified System.IO as IO
import Data.ListLike.Base

{- | An extension to 'ListLike' for those data types that support
I\/O.  These functions mirror those in "System.IO" for the most part.  They
also share the same names; see the comments in "Data.ListLike" for help
importing them.

Note that some types may not be capable of lazy reading or writing.
Therefore, the usual semantics of "System.IO" functions regarding laziness
may or may not be available from a particular implementation.

Minimal complete definition:

* hGetLine

* hGetContents

* hGet

* hGetNonBlocking

* hPutStr
-}
class (ListLike full item) => ListLikeIO full item | full -> item where
    {- | Reads a line from the specified handle -}
    hGetLine :: IO.Handle -> IO full

    -- | Read entire handle contents.  May be done lazily like
    -- 'System.IO.hGetContents'.
    hGetContents :: IO.Handle -> IO full

    -- | Read specified number of bytes.  See 'System.IO.hGet' for
    -- particular semantics.
    hGet :: IO.Handle -> Int -> IO full

    -- | Non-blocking read.  See 'System.IO.hGetNonBlocking' for more.
    hGetNonBlocking :: IO.Handle -> Int -> IO full

    -- | Writing entire data.
    hPutStr :: IO.Handle -> full -> IO ()

    -- | Write data plus newline character.
    hPutStrLn :: IO.Handle -> full -> IO ()
    hPutStrLn fp x =
        do hPutStr fp x
           IO.hPutStrLn fp ""

    -- | Read one line
    getLine :: IO full
    getLine = hGetLine IO.stdin

    -- | Read entire content from stdin.  See 'hGetContents'.
    getContents :: IO full
    getContents = hGetContents IO.stdin

    -- | Write data to stdout.
    putStr :: full -> IO ()
    putStr = hPutStr IO.stdout

    -- | Write data plus newline character to stdout.
    putStrLn :: full -> IO ()
    putStrLn = hPutStrLn IO.stdout

    -- | Interact with stdin and stdout by using a function to transform
    -- input to output.  May be lazy.  See 'System.IO.interact' for more.
    interact :: (full -> full) -> IO ()
    interact func =
        do c <- getContents
           putStr (func c)

    -- | Read file.  May be lazy.
    readFile :: FilePath -> IO full
    readFile fn =
        do fp <- IO.openFile fn IO.ReadMode
           hGetContents fp

    -- | Write data to file.
    writeFile :: FilePath -> full -> IO ()
    writeFile fn x =
        do fp <- IO.openFile fn IO.WriteMode
           hPutStr fp x
           IO.hClose fp

    -- | Append data to file.
    appendFile :: FilePath -> full -> IO ()
    appendFile fn x =
        do fp <- IO.openFile fn IO.AppendMode
           hPutStr fp x
           IO.hClose fp
