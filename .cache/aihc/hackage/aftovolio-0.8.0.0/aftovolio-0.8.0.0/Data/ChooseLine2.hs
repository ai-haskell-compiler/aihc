{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.ChooseLine2
-- Copyright   :  (c) Oleksandr Zhabenko 2021-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- General shared by phladiprelio-ukrainian-simple and phladiprelio-general-simple functionality to compare contents of the up to 14 files line-by-line 
-- and to choose the resulting option. 

{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Data.ChooseLine2 where

import GHC.Base
import Data.Foldable (mapM_) 
import Data.Maybe (fromMaybe) 
import Text.Show (Show(..))
import Text.Read (readMaybe)
import System.IO (putStrLn, FilePath,getLine,appendFile,putStr,readFile)
import Data.List
import Data.Tuple (fst,snd)
import Control.Exception (IOException,catch) 

-- | Is rewritten from the <https://hackage.haskell.org/package/phonetic-languages-simplified-examples-array-0.21.0.0/docs/src/Phonetic.Languages.Lines.html#compareFilesToOneCommon>
-- Given a list of different filepaths and the resulting filepath for the accumulated data provides a simple way to compare options of the lines in every file in the
-- first argument and to choose that option for the resulting file. Therefore, the resulting file can combine options for lines from various sources.
compareFilesToOneCommon 
 :: Int -- ^ A number of files to be read and treated as sources of lines to choose from.
 -> [FilePath] 
 -> FilePath 
 -> IO ()
compareFilesToOneCommon n files file3 = do
 contentss <- mapM ((\(j,ks) -> do {readFileIfAny ks >>= \fs -> return (j, zip [1..] . lines $ fs)})) . zip [1..] . take n $ files
 compareF contentss file3
   where compareF :: [(Int,[(Int,String)])] -> FilePath -> IO ()
         compareF ysss file3 = mapM_ (\i -> do
          putStr "Please, specify which variant to use as the result, "
          putStrLn "maximum number is the quantity of the files from which the data is read: "
          let strs = map (\(j,ks) -> (\ts -> if null ts then (j,"")
                      else let (_,rs) = head ts in  (j,rs)) .
                       filter ((== i) . fst) $ ks) ysss
          putStrLn . unlines . map (\(i,xs) -> show i ++ ":\t" ++ xs) $ strs
          ch <- getLine
          let choice2 = fromMaybe 0 (readMaybe ch::Maybe Int)
          toFileStr file3 ((\us -> if null us then [""] else [snd . head $ us]) . filter ((== choice2) . fst) $ strs)) $ [1..]

{-| Inspired by: 'https://hackage.haskell.org/package/base-4.15.0.0/docs/src/GHC-IO.html#catch' 
 Is taken from the 
https://hackage.haskell.org/package/string-interpreter-0.8.0.0/docs/src/Interpreter.StringConversion.html#readFileIfAny
to reduce general quantity of dependencies.
Reads a textual file given by its 'FilePath' and returns its contents lazily. If there is
some 'IOException' thrown or an empty file then returns just "". Raises an exception for the binary file. -}
readFileIfAny :: FilePath -> IO String
readFileIfAny file = catch (readFile file) (\(_ :: IOException) -> return "")
{-# INLINE readFileIfAny #-}

-- | Prints list of 'String's to the file as a multiline 'String' with default line ending. Uses 'appendFile' function inside.
toFileStr ::
  FilePath -- ^ The 'FilePath' to the file to be written in the 'AppendMode' (actually appended with) the information output.
  -> [String] -- ^ Each element is appended on the new line to the file.
  -> IO ()
toFileStr file = appendFile file . unlines
{-# INLINE toFileStr #-}

