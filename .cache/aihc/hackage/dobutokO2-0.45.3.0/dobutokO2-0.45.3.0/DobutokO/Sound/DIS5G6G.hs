-- |
-- Module      :  DobutokO.Sound.DIS5G6G
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.DIS5G6G (
  -- ** Auxiliary functions
  syllableStr 
  -- *** Working with Intervals, Durations, Strengths and StrengthDb
  , intervalsFromString 
  , vStrToVInt 
  , strToInt
  , durationsAver
  , str2Durat1
  , str2Durations
  , str2Vol1
  , str2Volume
  , doublesAveragedA
  , doublesAveragedG
  , equalize2Vec
  , intervalsFromStringG
  , silentSound2G
  , strengthsAver
  , strengthsDbAver
  -- * New generalized 6G functions that works with Strengths
  , apply6G
  , apply6G2
  , apply6GS
  , apply6GS2
) where

import CaseBi.Arr (getBFstLSorted')
import Numeric
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import System.Process
import EndOfExe2
import Aftovolio.Ukrainian.Melodics (convertToProperUkrainianI8)
import MMSyn7l
import DobutokO.Sound.IntermediateF
import DobutokO.Sound.Functional.Params
import DobutokO.Sound.Decibel

-- | Generatlized version of the 'intervalsFromString' with a possibility to specify your own 'Intervals'.
intervalsFromStringG :: Intervals -> String -> Intervals
intervalsFromStringG v = vStrToVIntG v . convertToProperUkrainian

-- | The default way to get 'Intervals' from a converted Ukrainian text.
vStrToVInt :: V.Vector String -> Intervals
vStrToVInt = V.map (strToIntG defInt)

-- | The default way to get number of semi-tones between notes in a single element of 'Intervals'.
strToInt :: String -> Int
strToInt = strToIntG defInt
{-# INLINE strToInt #-}

---------------------------------------------------------------------------------------------------------------------

-- | Arithmetic average for the 'V.Vector' is used as a weight for a duration. 
doublesAveragedA :: V.Vector Float -> Float -> V.Vector Float
doublesAveragedA v4 y3 
  | V.null v4 || y3 == 0 = V.empty
  | otherwise = let aver = V.sum v4 / fromIntegral (V.length v4) in if aver == 0.0 then doublesAveragedA (V.filter (/= 0.0) v4) y3 
      else V.map (\t4 -> t4 * y3 / aver) v4

-- | Geometric average for the 'V.Vector' is used as a weight for a strength. 
doublesAveragedG :: V.Vector Float -> Float -> V.Vector Float
doublesAveragedG v4 y3 
  | V.null v4 || y3 == 0 = V.empty
  | otherwise = let aver = V.product v4 ** (fromIntegral 1 / (fromIntegral (V.length v4))) in if aver == 0.0 then doublesAveragedG (V.filter (/= 0.0) v4) y3 
      else V.map (\t4 -> t4 * y3 / aver) v4      

-- | 'Durations' accounting the desired average duration.
durationsAver :: Durations -> Float -> Durations
durationsAver = doublesAveragedA

-- | 'Strengths' accounting the desired average strength.
strengthsAver :: Strengths -> Float -> Strengths
strengthsAver = doublesAveragedG

-- | 'StrengthsDb' accounting the desired average strength in dB.
strengthsDbAver :: StrengthsDb -> Float -> StrengthsDb
strengthsDbAver = doublesAveragedG

-- | Auxiliar function to make all vectors in a 'V.Vector' equal by length (the minimum one).
equalize2Vec :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
equalize2Vec v = let min = V.minimum . V.map V.length $ v in V.map (V.unsafeSlice 0 min) v

-- | A full conversion to the 'Durations' from a Ukrainian text.
str2Durations :: String -> Float -> Durations
str2Durations xs y 
 | compare y 0.0 == GT && not (null xs) = durationsAver (V.map str2Durat1 . convertToProperUkrainian $ xs) y
 | otherwise = V.empty

-- | A conversion to the 'Float' that is used inside 'str2Durations'.
str2Durat1 :: String -> Float
str2Durat1 = getBFstLSorted' (-0.153016) [("-", (-0.101995)), ("0", (-0.051020)), ("1", (-0.153016)), ("а", 0.138231), ("б", 0.057143), 
  ("в", 0.082268), ("г", 0.076825), ("д", 0.072063), ("дж", 0.048934), ("дз", 0.055601), ("е", 0.093605), ("ж", 0.070658), ("з", 0.056054), 
    ("и", 0.099955), ("й", 0.057143), ("к", 0.045351), ("л", 0.064036), ("м", 0.077370), ("н", 0.074240), ("о", 0.116463), ("п", 0.134830), 
      ("р", 0.049206), ("с", 0.074603), ("сь", 0.074558), ("т", 0.110658), ("у", 0.109070), ("ф", 0.062268), ("х", 0.077188), ("ц", 0.053061), 
        ("ць", 0.089342), ("ч", 0.057596), ("ш", 0.066077), ("ь", 0.020227), ("і", 0.094150), ("ґ", 0.062948)]

-- | A full conversion to the 'Strengths' from a Ukrainian text.
str2Volume :: String -> Strengths
str2Volume = V.map (getBFstLSorted' 0.0 [("а", 0.890533), ("б", 0.211334), ("в", (-0.630859)), ("г", (-0.757599)), ("д", 0.884613), ("дж", 0.768127), 
  ("дз", (-0.731262)), ("е", (-0.742523)), ("ж", (-0.588959)), ("з", (-0.528870)), ("и", 0.770935), ("й", (-0.708008)), ("к", (-0.443085)), 
    ("л", 0.572632), ("м", (-0.782349)), ("н", (-0.797607)), ("о", (-0.579559)), ("п", 0.124908), ("р", 0.647369), ("с", 0.155640), ("сь", (-0.207764)), 
      ("т", -0.304443), ("у", 0.718262), ("ф", (-0.374359)), ("х", (-0.251160)), ("ц", (-0.392365)), ("ць", 0.381348), ("ч", (-0.189240)), 
        ("ш", 0.251221), ("ь", 0.495483), ("і", (-0.682709)), ("ґ", 0.557098)]) . convertToProperUkrainian

-- | A conversion to the 'Float' that is used inside 'str2Volume'.
str2Vol1 :: String -> Float
str2Vol1 = getBFstLSorted' 0.0 [("а", 0.890533), ("б", 0.211334), ("в", (-0.630859)), ("г", (-0.757599)), ("д", 0.884613), ("дж", 0.768127), 
  ("дз", (-0.731262)), ("е", (-0.742523)), ("ж", (-0.588959)), ("з", (-0.528870)), ("и", 0.770935), ("й", (-0.708008)), ("к", (-0.443085)), 
    ("л", 0.572632), ("м", (-0.782349)), ("н", (-0.797607)), ("о", (-0.579559)), ("п", 0.124908), ("р", 0.647369), ("с", 0.155640), ("сь", (-0.207764)), 
      ("т", -0.304443), ("у", 0.718262), ("ф", (-0.374359)), ("х", (-0.251160)), ("ц", (-0.392365)), ("ць", 0.381348), ("ч", (-0.189240)), 
        ("ш", 0.251221), ("ь", 0.495483), ("і", (-0.682709)), ("ґ", 0.557098)] . V.unsafeHead . convertToProperUkrainian

-- | For the given non-existing 'FilePath' for a sound file supported by SoX generates a silence of the specified duration and quality (see, 
-- 'soxBasicParams').
silentSound2G :: FilePath -> Float -> String -> IO ()
silentSound2G file y4 ys = do
  _ <- readProcessWithExitCode (fromJust (showE "sox")) 
     ((if null ys then id else soxBasicParams ys) ["-r22040","-n",file,"synth", showFFloat (Just 1) y4 "","sine","440.0","vol","0"]) ""
  putStr ""

-- | After producing sounds as WAV or FLAC files you can apply to them volume adjustments using 'Strengths'. The first 'String' is used accordingly to 
-- 'soxBasicParams' and the second one -- as a prefix of the filenames for the files that the function is applied to. The files must not be silent ones. 
-- Otherwise, it leads to likely noise sounding or errors.
apply6G :: Strengths -> String -> String -> IO ()
apply6G v6 ys zs 
 | V.null v6 = putStrLn "Nothing changed, because the vector of volume adjustments is empty! "
 | otherwise = do
     dir0v <- listVDirectory3G ys zs  
     V.imapM_ (\i file -> soxE file ["norm","vol", showFFloat (Just 4) (V.unsafeIndex v6 (i `rem` V.length v6)) ""]) dir0v

-- | Variant of the 'apply6G' where you use as a 'Strengths' parameter that one obtained from a Ukrainian text provided as a first 'String' argument. 
-- It uses 'str2Volume' inside. The files must not be silent ones. Otherwise, it leads to likely noise sounding or errors.
apply6GS :: String -> String -> String -> IO ()
apply6GS xs = apply6G (str2Volume xs)

-- | Variant of the 'apply6G' function which can be applied also to the silent files. Whether a file is silent is defined using the 'Float' argument 
-- so that if the maximum by absolute value amplitude is less by absolute value than the 'Float' argument then the file is not changed.
apply6G2 :: Strengths -> String -> String -> Float -> IO ()
apply6G2 v6 ys zs limV
 | V.null v6 = putStrLn "Nothing changed, because the vector of volume adjustments is empty! "
 | otherwise = do
     dir0v <- listVDirectory3G ys zs  
     V.imapM_ (\i file -> apply6GSilentFile file limV (V.unsafeIndex v6 (i `rem` V.length v6))) dir0v

-- | Variant of the 'apply6G2' where you use as a 'Strengths' parameter that one obtained from a Ukrainian text provided as the first 'String' argument. 
-- It uses 'str2Volume' inside. 
apply6GS2 :: String -> String -> String -> Float -> IO ()
apply6GS2 xs = apply6G2 (str2Volume xs)

