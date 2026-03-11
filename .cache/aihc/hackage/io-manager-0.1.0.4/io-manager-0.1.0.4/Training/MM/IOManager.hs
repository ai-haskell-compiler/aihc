{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  $Header$
Description :  Main module of io-manager library
Copyright   :  (c) Mihai Maruseac
License     :  BSD3

Maintainer  :  mihai.maruseac@gmail.com
Stability   :  stable
Portability :  portable

A skeleton library to help learners of Haskell concentrate on the
pure-functional aspect and let the IO be handled by the library.
-}
module Training.MM.IOManager
  (
    -- * The @Filename@ type
    Filename

    -- * The @Input@ type
  , Input

    -- * The @Output@ type
  , Output

    -- * Exported for use in main pipeline
  , wrapIO

    -- * Exported to be usable by students
  , getStdIn
  , getInputFile
  , writeStdOut
  , writeStdErr
  , writeOutputFile
  ) where

import qualified Data.Map as Map
import Control.Monad (liftM)
import System.Environment (getArgs)
import qualified System.IO as System

-- | Type of filenames.
type Filename = String

-- | Type of values holding inputs to the program, grouped by input source.
data Input = Input
  { stdin :: String
  , fileInput :: Map.Map Filename String
  }

-- | Type of values holding outputs of the program, grouped by output source.
data Output = Output
  { stdout :: String
  , stderr :: String
  , fileOutput :: Map.Map Filename String
  }

-- | Obtains the contents of the standard input as given to the program.
-- Returns a String containing the input without any modification.
getStdIn :: Input -> String
getStdIn = stdin

-- | Obtains the contents of an input file. Returns a String containing the
-- input without any modification.
getInputFile :: Input -> Filename -> String
getInputFile i f = fileInput i Map.! f

-- | Appends text to the standard output. No newline is printed at the end,
-- the caller must handle it. Returns a new @Output@ value, containing the
-- appended text.
writeStdOut :: Output -> String -> Output
writeStdOut o@Output{..} s = o { stdout = stdout ++ s }

-- | Appends text to the standard error. No newline is printed at the end, the
-- caller must handle it. Returns a new @Output@ value, containing the
-- appended text.
--
-- Note: When running the program, the standard error text is displayed
-- after the entire text from the standard input is displayed.
writeStdErr :: Output -> String -> Output
writeStdErr o@Output{..} s = o { stderr = stderr ++ s }

-- | Appends to an output file. If the file does not exist in the @Output@
-- value (this program didn't yet write in it), it is created as a new one.
-- Returns a new @Output@ value, containing the appended text.
writeOutputFile :: Output -> Filename -> String -> Output
writeOutputFile o@Output{..} f s
  = o { fileOutput = Map.insertWith (++) f s fileOutput }

-- | Reads the input from all the files given as command line arguments and
-- constructs an @Input@ value.
readInput :: IO (Input, Output)
readInput = do
  args <- getArgs
  imap <- readInputFiles args Map.empty
  input <- getContents
  return (Input input imap, Output "" "" Map.empty)

-- | Writes the contents of an @Output@ value to the needed files.
writeOutput :: Output -> IO ()
writeOutput o = do
  putStrLn $ stdout o
  System.hPutStrLn System.stderr $ stderr o
  writeOutputFiles $ Map.toList $ fileOutput o

-- | Wraps a simple function @Input@ -> @Output@ -> @Output@ in
-- order to simplify student's usage.
wrapIO :: (Input -> Output -> Output) -> IO ()
wrapIO f = liftM (uncurry f) readInput >>= writeOutput

-- | Reads all of the input files into the map of the Input value.
readInputFiles :: [Filename]
               -> Map.Map Filename String
               -> IO (Map.Map Filename String)
readInputFiles [] m = return m
readInputFiles (f:fs) m = do
  content <- readFile f
  readInputFiles fs $ Map.insert f content m

-- | Writes the content of all the output files.
writeOutputFiles :: [(Filename, String)] -> IO ()
writeOutputFiles [] = return ()
writeOutputFiles ((f,s):fs) = writeFile f s >> writeOutputFiles fs
