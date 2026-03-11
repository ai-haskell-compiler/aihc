-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2021-2025
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
--

{-# OPTIONS_GHC -threaded -rtsopts #-}
{-# OPTIONS_HADDOCK -show-extensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE BangPatterns #-}

module Main where

import GHC.Base
import Aftovolio.Ukrainian.ReverseConcatenations
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.IO
import GHC.List
import Data.Char (isDigit)

main :: IO ()
main = do
  args0 <- getArgs
  let !fstA = fromMaybe 1 (readMaybe (mconcat . take 1 $ args0)::Maybe Int)
      !args
        | fstA == 1 = filter (/= "-i") args0
        | otherwise = filter (/= "-i") . drop 1 $ args0
      !stdinput = any (== "-i") args0 -- If you specify \"-i\" then reads the input text from the stdin otherwise from the file specified instead.
      !filterMusicInformation = any (== "-m") args -- ^ If you specify as one of the command line arguments \"-m\" then the digits and symbols like \'=\' and \'_\' are filtered, so cleaning from the music information is done.
  if stdinput then do
     contents <- getContents
     putStrLn . reverseConcatenations fstA . (if filterMusicInformation then filter (\c -> (not . isDigit $ c) && c /= '=' && c /= '_') else id) $ contents
  else do
     let !file = concat . take 1 $ args
         !toFile = concat . drop 1 . take 2 $ args
     if null file then putStrLn "No file to read the Ukrainian text is specified. "
     else readFile file >>= return . reverseConcatenations fstA  . (if filterMusicInformation then filter (\c -> (not . isDigit $ c) && c /= '=' && c /= '_') else id) >>= (\ts -> if null toFile then putStrLn ts else writeFile toFile ts)
