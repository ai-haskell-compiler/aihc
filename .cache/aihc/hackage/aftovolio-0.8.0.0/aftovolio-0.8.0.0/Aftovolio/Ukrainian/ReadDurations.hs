{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Aftovolio.Ukrainian.ReadDurations
Copyright   :  (c) OleksandrZhabenko 2021-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Functions to read the properties data from the files with the special Haskell-like syntaxis.
-}
module Aftovolio.Ukrainian.ReadDurations where

import Aftovolio.General.Datatype3 (zippedDouble2Word8)
import Aftovolio.Ukrainian.Melodics
import Aftovolio.Ukrainian.SyllableWord8
import CaseBi.Arr (getBFstLSorted')
import Data.List (lines, unlines)
import Data.Maybe
import GHC.Base
import GHC.List
import GHC.Word
import System.Directory (doesFileExist)
import System.IO
import Text.Read (readMaybe)

{- | For more information on implementation, please refer to the link:

<https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#ability-to-use-your-own-durations-of-representations-of-sounds-or-phonetic-phenomena>
-}
sound8s :: FlowSound
sound8s =
    [ 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 10
    , 11
    , 15
    , 16
    , 17
    , 18
    , 19
    , 20
    , 21
    , 22
    , 23
    , 24
    , 25
    , 26
    , 27
    , 28
    , 29
    , 30
    , 31
    , 32
    , 33
    , 34
    , 35
    , 36
    , 37
    , 38
    , 39
    , 40
    , 41
    , 42
    , 43
    , 44
    , 45
    , 46
    , 47
    , 48
    , 49
    , 50
    , 51
    , 52
    , 53
    , 54
    , 66
    , 101
    ]

{- |

Since the version 0.5.0.0 the semantics changed. Now, there is no duration for the pause between words.
The 52 'Double' numbers become the durations of the above specified 'Sound8' values respectively, the order
must be preserved (if you consider it important, well, it should be!). If some number in the file cannot be read
as a 'Double' number the function uses the first one that can be instead (the default value). If no such is specified
at all, then the default number is 5 for all the 'Sound8' sound representations.
-}
readSound8ToWord8 :: String -> (Word8, [(Sound8, Word8)])
readSound8ToWord8 xs
    | null xs = (5, zip sound8s . replicate 10000 $ 5)
    | otherwise =
        let wws = lines xs
            dbls = map (\ks -> readMaybe ks :: Maybe Double) wws
            dbSs = map (fromMaybe 5) dbls
         in (5, zippedDouble2Word8 . zip sound8s $ dbSs `mappend` replicate 10000 1.0)

divide2SDDs :: String -> [String]
divide2SDDs ys
    | null tss = [unlines kss]
    | otherwise = unlines kss : divide2SDDs (unlines rss)
  where
    wwss = lines ys
    (kss, tss) = break (any (== '*')) wwss
    rss = dropWhile (any (== '*')) tss

readSyllableDurations :: FilePath -> IO [[[[Sound8]]] -> [[Word8]]]
readSyllableDurations file = do
    exists <- doesFileExist file
    if exists
        then do
            xs <- readFile file
            let yss = take 9 . divide2SDDs $ xs
                readData = map readSound8ToWord8 yss
            return . map (\(d, zs) -> syllableDurationsGDc (getBFstLSorted' d zs)) $ readData
        else return []
