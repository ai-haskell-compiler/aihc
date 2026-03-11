-- |
-- Module      :  DobutokO.Sound.Process
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Process (
  -- * Basic functions for the executable
  recAndProcess
  , processD1
  , processD2
  , d3H
  , d4H
  , d5H
  , d7H
  , d8H
  , d9H
) where

import Data.List (isPrefixOf)
import CaseBi.Arr (getBFstLSorted')
import Numeric (showFFloat)
import Control.Exception (onException)
import Data.Maybe (fromJust)
import Data.Char (isDigit,isSpace)
import System.Process
import EndOfExe2 (showE)
import qualified Data.Vector as V (fromList)
import System.Directory
import Sound.SoXBasics
import SoXBasics.Arr (recA)
import Processing_mmsyn7ukr_array

-- | Function records and processes the sound data needed to generate the \"end.wav\" file in the 'dobutokO2' function. Please, check before executing
-- whether there is no \"x.wav\" file in the current directory, because it can be overwritten.
recAndProcess :: FilePath -> Int -> IO String
recAndProcess file =
  getBFstLSorted' processD [(0,processD0 file),(1,processD1),(2,processD2 file),(3,processD3),(4,processD4),(5,processD5),(7,processD7),
    (8,processD8),(9,processD9),(11,processD_1),(99,processD99),(999,processD99)]

processD_1 :: IO String
processD_1 = onException (do
  putStr "Please, specify two \'Int\' numbers (with intermediate space character between them): the first one is a number of different notes there will be "
  putStr "in the result, and the second one is a number of enky, to which you would like all the main components (not taking into account their "
  putStr "respective lower bases of the intervals if any will exist) should belong. "
  putStrLn "If you specify as the first one 2 (possibly the simplest case), then to the second one you can define a number in the range [3..53]. "
  putStrLn "If you specify as the first one 3, then to the second one you can define a number in the range [2..35]. "
  putStrLn "If you specify as the first one 4, then to the second one you can define a number in the range [2..26]. "
  putStrLn "If you specify as the first one 6, then to the second one you can define a number in the range [1..17]. "
  putStrLn "If you specify as the first one 9, then to the second one you can define a number in the range [1..11]. "
  enka0 <- getLine
  let enka1 = take 2 . words . filter (\x -> isDigit x || isSpace x) $ enka0
      enka2 = read (head . take 1 $ enka1)::Int
      enka3
        | enka2 == 2 = if compare ((read (take 2 . head . tail $ enka1)::Int) `rem` 53) 3 == LT then 28 else (read (take 2 . head . tail $ enka1)::Int)
           `rem` 53
        | enka2 == 3 = if compare ((read (take 2 . head . tail $ enka1)::Int) `rem` 35) 2 == LT then 19 else (read (take 2 . head . tail $ enka1)::Int)
           `rem` 35
        | enka2 == 4 = if compare ((read (take 2 . head . tail $ enka1)::Int) `rem` 26) 2 == LT then 14 else (read (take 2 . head . tail $ enka1)::Int)
           `rem` 26
        | enka2 == 6 = if compare ((read (take 2 . head . tail $ enka1)::Int) `rem` 17) 1 == LT then 9 else (read (take 2 . head . tail $ enka1)::Int)
           `rem` 17
        | enka2 == 9 = if compare ((read (take 2 . head . tail $ enka1)::Int) `rem` 11) 1 == LT then 6 else (read (take 2 . head . tail $ enka1)::Int)
           `rem` 11
        | otherwise  = error "Not valid number in the second place. "
  return $ show enka2 ++ " " ++ show enka3 ) (do
    putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
    putStrLn "_______________________________________________________________________"
    processD3)
{-# INLINE processD_1 #-}

processD0 :: FilePath -> IO String
processD0 file = onException (readProcessWithExitCode (fromJust (showE "sox")) [file, "x.wav", "-r22050", "channels", "1"] "" >> putStrLn "" >> return "") (do
  exist <- doesFileExist "x.wav"
  if exist then removeFile "x.wav"
  else putStr ""
  putStrLn ""
  putStr "The process was not successful may be because of the not valid data OR SoX cannot convert the given file to the .wav format. "
  putStrLn "Interrupt the program and start again with the valid file. "
  putStrLn "_______________________________________________________________________"
  processD0 file)
{-# INLINE processD0 #-}
  
processD1 :: IO String
processD1 = onException (do
  tempeRa 0
  putStrLn "Please, specify, how many seconds long sound data you would like to record."
  time <- getLine
  let time0 = read (filter (\t -> isDigit t || t == '.') time)::Float
  putStrLn "Please, wait for 0.5 second and produce the needed sound now."
  recA "x.wav" time0
  putStrLn ""
  return "") (do
    dir0 <- listDirectory "."
    let paths5 = filter (isPrefixOf "nx.") dir0
        paths6 = filter (== "x.wav") dir0
        paths  = paths5 ++ paths6
    mapM_ removeFile paths
    putStrLn ""
    putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
    putStrLn "_______________________________________________________________________"
    processD1)
{-# INLINE processD1 #-}

processD2 :: FilePath -> IO String
processD2 file = onException (do     
  exist3 <- doesFileExist file
  if exist3 then return ""
  else do
    putStr "Please, specify the control parameter for the SoX \"noisered\" effect in the range from 0.0 to 1.0. "
    putStrLn "The greater value causes more reduction with possibly removing some important sound data. The default value is 0.5 "
    putStrLn "To use the default value, you can simply press Enter."
    ctrlN <- getLine
    let addit = dropWhile (/= '.') . filter (\t -> isDigit t || t == '.') $ ctrlN
        noiseP = if null ctrlN then ""
                 else tail addit
    controlNoiseReduction $ '0':noiseP 
    norm "_x.wav" 
    if "nx." `isPrefixOf` file 
      then putStr "" 
      else renameFile "8_x.wav" file 
    removeFile "x.wav" 
    removeFile "_x.wav" 
    dir <- listDirectory "." 
    let paths4 = filter (isPrefixOf "nx.") dir
    mapM_ removeFile paths4 
    putStrLn "" 
    return "") (do
      putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
      putStrLn "_______________________________________________________________________"
      processD2 file)
{-# INLINE processD2 #-}

processD3 :: IO String
processD3 = onException (do
  putStr "Please, specify the octave number, to which you would like all the main components (not taking into account their respective lower pure quints) "
  putStrLn "should belong. The number should be better in the range [1..8]"
  fmap d3H getLine) (do
    putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
    putStrLn "_______________________________________________________________________"
    processD3)
{-# INLINE processD3 #-}

d3H :: String -> String
d3H xs = show $ (read (take 1 xs)::Int) `rem` 9
{-# INLINE d3H #-}

processD4 :: IO String
processD4 = onException (do
  putStr "Please, specify the amplitude for the generated overtones as an Int number in the range [0..99]. "
  putStrLn "The default one is 99"
  putStrLn "To use the default value, you can simply press Enter."
  fmap d4H getLine) (do
             putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
             putStrLn "_______________________________________________________________________"
             processD4)
{-# INLINE processD4 #-}

d4H :: String -> String
d4H xs
 | null xs = "1.0"
 | otherwise = let amplOb = (read (take 2 . filter isDigit $ xs)::Int) `rem` 100 in
    case amplOb of
      99 -> "1.0"
      _ -> if compare (amplOb `quot` 9) 1 == LT then "0.0" ++ show (amplOb + 1)
           else "0." ++ show (amplOb + 1)
{-# INLINE d4H #-}

processD5 :: IO String
processD5 = onException (do
  putStr "Please, specify the basic duration for the generated sounds as a Float number in the range [0.1..4.0]. "
  putStrLn "The default one is 0.5"
  putStrLn "To use the default value, you can simply press Enter."
  fmap d5H getLine) (do
             putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
             putStrLn "_______________________________________________________________________"
             processD5)
{-# INLINE processD5 #-}

d5H :: String -> String
d5H xs
  | null xs = "0.5"
  | otherwise = let time1 = (read (filter (\z -> isDigit z || z == '.') xs)::Float) in
      if compare time1 0.1 /= LT && compare time1 4.0 /= GT then showFFloat (Just 4) time1 $ show 0
      else let mantissa = time1 - (fromIntegral . truncate $ time1)
               ceilP    = (truncate time1::Int) `rem` 4 in
             if ceilP == 0 then "0." ++ showFFloat (Just 4) mantissa (show 0)
             else show ceilP ++ "." ++ showFFloat (Just 4) mantissa (show 0)
{-# INLINE d5H #-}

processD7 :: IO String
processD7 = onException (do
  putStrLn "Please, input the Ukrainian text that will be used to define signs for the harmonics coefficients to produce a special timbre for the notes: "
  fmap d7H getLine) (do
    putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
    putStrLn "_______________________________________________________________________"
    processD7)
{-# INLINE processD7 #-}

d7H :: String -> String
d7H xs
  | null xs = "або"
  | otherwise = xs
{-# INLINE d7H #-}

processD8 :: IO String
processD8 = onException (do
  putStr "Please, specify in how many times the amplitude for the second lower note (if any) is greater than the amplitude for the main note. "
  putStrLn "The number is in the range [0.1..2.0]. The default one is 1.0"
  putStrLn "To use the default value, you can simply press Enter."
  fmap d8H getLine) (do
             putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
             putStrLn "_______________________________________________________________________"
             processD8)
{-# INLINE processD8 #-}

d8H :: String -> String
d8H xs
  | null xs = "1.0"
  | otherwise = let dAmpl1 = (read (filter (\z -> isDigit z || z == '.') xs)::Float) in
    if compare dAmpl1 0.1 /= LT && compare dAmpl1 2.0 /= GT then showFFloat (Just 4) dAmpl1 $ show 0
    else let mantissa = dAmpl1 - (fromIntegral . truncate $ dAmpl1)
             ceilP    = (truncate dAmpl1::Int) `rem` 2 in
           if ceilP == 0 then "0." ++ showFFloat (Just 4) mantissa (show 0)
           else show ceilP ++ "." ++ showFFloat (Just 4) mantissa (show 0)
{-# INLINE d8H #-}

processD9 :: IO String
processD9 = onException (do
  putStrLn "Please, input the Ukrainian text that will be used to define the intervals to be used to produce the lower note for the given main one. "
  putStrLn "The default one is \"й\". "
  putStrLn "To use the default value, you can simply press Enter."
  fmap d9H getLine) (do
    putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
    putStrLn "_______________________________________________________________________"
    processD9)
{-# INLINE processD9 #-}

d9H :: String -> String
d9H xs
  | null xs = "й"
  | otherwise = xs
{-# INLINE d9H #-}

processD :: IO String
processD = onException (do
  putStrLn "Please, input the Ukrainian text that will be used to create a special timbre for the notes: "
  getLine) (do
    putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
    putStrLn "_______________________________________________________________________"
    processD)
{-# INLINE processD #-}

processD99 :: IO String
processD99 = onException (do
  putStr "Please, input the lists of Int in Haskell syntaxis (e. g. [1,3..56], or [3..45], or [2..]) of the indices for the files to be played "
  putStr "with SoX effects applied to. The lists must be separated with newline (just press \"Enter\"), empty lists are ignored. If index is an "
  putStrLn "element of several input lists then if its number of occurrences in all the lists is odd, then it is played, otherwise it is not. "
  putStrLn "To end the input, just press the combination that means the end of input (e. g. for Unices, it's probably Ctrl + D). "
  getContents) (do
    putStrLn "The process was not successful may be because of the not valid data. Please, specify the valid data as requested."
    putStrLn "_______________________________________________________________________"
    processD)
{-# INLINE processD99 #-}

----------------------------------------------------------------------------------------------   

