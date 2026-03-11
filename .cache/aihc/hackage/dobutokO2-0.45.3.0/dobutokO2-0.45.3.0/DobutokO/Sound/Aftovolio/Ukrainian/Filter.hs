-- |
-- Module      :  DobutokO.Sound.Aftovolio.Ukrainian.Filter
-- Copyright   :  (c) OleksandrZhabenko 2025
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Provides an easy way to create some rhythmic patterns from a Ukrainian text. 
-- Uses aftovolioUkr executable inside from the @aftovolio@ package.

{-# LANGUAGE BangPatterns, NoImplicitPrelude #-}
{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Aftovolio.Ukrainian.Filter (
  -- * Library and executable functions
  quantizeDurationsBasedOnUkrainianText
) where

import System.Exit (ExitCode(..))
import GHC.Base
import GHC.Num ((*),(-))
import GHC.Real (fromIntegral)
import Data.List
import System.IO (FilePath,hPutStrLn,stderr)
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import System.Process (readProcessWithExitCode)
import EndOfExe2 (showE)
import Aftovolio.Halfsplit
import Aftovolio.General.Datatype3
import Aftovolio.Ukrainian.ReadDurations (readSyllableDurations)
import Aftovolio.Ukrainian.Syllable
import Aftovolio.Ukrainian.SyllableWord8
import Aftovolio.Ukrainian.Melodics
import TwoQuantizer (twoQuantizerG)

{-|
   'quantizeDurationsBasedOnUkrainianText' processes and quantizes syllable durations derived from a Ukrainian text that can be further used by DobutokO modules for creating music and sound patterns or timbre.

  This function performs several steps to produce a list of quantized durations. 

  It reads syllable durations, invoke aftovolioUkr executable with argements, processes and parses output and makes quantization of the result.

  In the event that the external process does not exit successfully, the function writes the error message to stderr
  and returns an empty list. -}
quantizeDurationsBasedOnUkrainianText 
  :: FilePath -- ^ A 'FilePath' to a file containing syllable durations. If empty, a default set of durations is selected based on @k@ (the 'Int' argument here).
  -> Int -- ^ An 'Int' used to select the specific group of durations from the file (if available) or determine which default list to use.
  -> [Double] -- ^ A list of 'Double' values representing quantization parameters. The resulting list will contain just the values from this list.
  -> String -- ^ A 'String' containing the Ukrainian text to be processed. This text is split into words and passed to the external executable @aftovolioUkr@.
  -> [String] -- ^ A list of additional command-line arguments (['String']) to be forwarded to the @aftovolioUkr@ executable.
  -> [String] -- ^ A list of 'String' values representing selected line numbers or indices to influence the processing of the Ukrainian text. If empty then the whole output is used without selection. If specified in the modes without tests and file single line output changes the output so that just the lines with the specified Int numbers are displayed in the order of the specified numbers. To specify some range, use just dash as here: \"34-250\" meaning that lines with numbers between 34 and 250 inclusively will be displayed. The output preserves music mode additional information as well here.
  -> IO [Double]
quantizeDurationsBasedOnUkrainianText file k quants ukrstrs argss lineNumbersSels = do
    syllableDurationsDs <- readSyllableDurations file
    (code, stdout0, strerr0) <- readProcessWithExitCode (fromJust (showE "aftovolioUkr")) (argss ++ concat [["+nm"], if null lineNumbersSels then ["1-362880"] else lineNumbersSels, ["-nm"]] ++ words ukrstrs) ""
    if code == ExitSuccess then do
        let basicDurations = map fromIntegral . read3
                        (not . null . filter (not . isSpace))
                        1.0
                        ( mconcat
                            . ( if null file
                                    then case k of
                                        1 -> syllableDurationsD
                                        3 -> syllableDurationsD3
                                        4 -> syllableDurationsD4
                                        _ -> syllableDurationsD2
                                    else
                                        if length syllableDurationsDs >= k
                                            then syllableDurationsDs !! (k - 1)
                                            else syllableDurationsD2
                              )
                            . createSyllablesUkrS
                        ) . unwords . lines $ stdout0
            quantizedDurations = twoQuantizerG False (\x y z -> compare (z*z) (x*y)) quants basicDurations
        return quantizedDurations
    else hPutStrLn stderr strerr0 >> return []
