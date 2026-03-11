-- |
-- Module      :  DobutokO.Sound.Uniq
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Uniq (
  -- * Library and executable functions
  -- ** For the unique for the String structure timbre
  uniqOvertonesV
  , uniqOverSoXSynth
  , uniqOverSoXSynthN
  -- *** For the unique for the String structure timbre with different signs for harmonics coefficients
  , uniqOvertonesV2
  , uniqOverSoXSynth2
  , uniqOverSoXSynthN3
  , uniqOverSoXSynthN4
  -- *** Use a file for information
  , uniqOverSoXSynthNGen
  , uniqOverSoXSynthNGen3
  , uniqOverSoXSynthNGen4
  -- * Extended generation using enky functionality
 -- ** With usage of additional information in the Ukrainian text
  , uniqOverSoXSynthNGenE
  , uniqOverSoXSynthNGen3E
  , uniqOverSoXSynthNGen4E
  -- * New 4G functions to work with Durations
  , uniqOverSoXSynthN4G
  , uniqOverSoXSynthN34G
  , uniqOverSoXSynthN44G
  , uniqOverSoXSynthNGenE4G
  , uniqOverSoXSynthNGen3E4G
  , uniqOverSoXSynthNGen4E4G
  -- ** 4G with speech-like composition
  , uniqOverSoXSynthN4GS
  , uniqOverSoXSynthN34GS
  , uniqOverSoXSynthN44GS
  , uniqOverSoXSynthNGenE4GS
  , uniqOverSoXSynthNGen3E4GS
  , uniqOverSoXSynthNGen4E4GS
  -- * New 5G functions to work also with Intervals
  , uniqOverSoXSynthN45G
  , uniqOverSoXSynthNGen4E5G
  -- ** 5G with obtained from the text arbitraty length Intervals
  , uniqOverSoXSynthN45GS
  , uniqOverSoXSynthNGen4E5GS
  -- * New 6G function to work also with Strengths
  , uniqOverSoXSynthNGen4E6G
  -- ** 6G with obtained from the text arbitrary length Strengths
  , uniqOverSoXSynthN46GS
  , uniqOverSoXSynthN46GSu
  , uniqOverSoXSynthNGen4E6GS
  , uniqOverSoXSynthNGen4E6GSu
  -- ** With overtones obtained from the additional Ukrainian text
  , uniqOverSoXSynthNGenEPar
  , uniqOverSoXSynthNGenE4GSPar
  , uniqOverSoXSynthNGenE4GPar
  , uniqOverSoXSynthNGen3EPar
  , uniqOverSoXSynthNGen3E4GSPar
  , uniqOverSoXSynthNGen3E4GPar
  , uniqOverSoXSynthNGen4EPar
  , uniqOverSoXSynthNGen4E4GSPar
  , uniqOverSoXSynthNGen4E4GPar
  , uniqOverSoXSynthNGen4E5GPar
  , uniqOverSoXSynthNGen4E5GSPar
  , uniqOverSoXSynthNGen4E6GPar
  , uniqOverSoXSynthNGen4E6GSPar
  , uniqOverSoXSynthNGen4E6GSuPar
) where

import Numeric (showFFloat)
import Data.Maybe (isNothing,fromJust)
import qualified Data.Vector as V
import System.Process
import EndOfExe2 (showE)
import Aftovolio.UniquenessPeriodsG
import Aftovolio.Ukrainian.Melodics
import DobutokO.Sound.Functional.Basics
import DobutokO.Sound.Functional.Params
import DobutokO.Sound.DIS5G6G

-- | For the given frequency of the note it generates a 'V.Vector' of the tuples, each one of which contains the harmonics' frequency and amplitude. For every given
-- 'String' structure of the uniqueness (see the documentation for @mmsyn7s@ and @uniqueness-periods@ packages) it produces the unique timbre.
uniqOvertonesV :: Float -> String -> OvertonesO
uniqOvertonesV note xs =
  let ys = map fromIntegral . uniquenessPeriodsGI8 [100,101,102] . convertToProperUkrainianI8 $ xs :: [Int]
      z  = sum ys
      v  = V.fromList . fmap (\y -> fromIntegral y / fromIntegral z) $ ys
      z2 = V.length v
      v2 = V.generate z2 (\i -> V.unsafeIndex v i / fromIntegral (i + 1)) in
        V.takeWhile (\(!u,!z) -> compare u (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) . V.unsafeSlice 1 (z2 - 1) .
          V.zip (V.generate z2 (\i -> note * fromIntegral (i + 1))) $ v2

-- | For the given frequency of the note it generates a 'V.Vector' of the tuples, each one of which contains the harmonics' frequency and amplitude. For every given
-- first 'String' argument structure of the uniqueness (see the documentation for @mmsyn7s@ and @uniqueness-periods@ packages) it produces the unique timbre.
-- The second 'String' is used to produce the signs for harmonics coefficients.
uniqOvertonesV2 :: Float -> String -> String -> OvertonesO
uniqOvertonesV2 note xs ts = 
  let ys = map fromIntegral . uniquenessPeriodsGI8 [100,101,102] . convertToProperUkrainianI8 $ xs :: [Int]
      z  = sum ys
      v  = V.fromList . fmap (\y -> fromIntegral y / fromIntegral z) $ ys
      z2 = V.length v
      v2 = V.generate z2 (\i -> (V.unsafeIndex (V.map fromIntegral . signsFromString z2 $ ts) i) * V.unsafeIndex v i / fromIntegral (i + 1)) in
        V.takeWhile (\(!u,!z) -> compare u (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) . V.filter (\(_, t4) -> t4 /= 0.0) .
          V.unsafeSlice 1 (z2 - 1) . V.zip (V.generate z2 (\i -> note * fromIntegral (i + 1))) $ v2
  
-- | For the given frequency and a Ukrainian text it generates a musical sound with the timbre obtained from the Ukrainian text (see the
-- documentation for @mmsyn7s@ package). The timbre for another given text usually differs, but can be the same. The last one is only
-- if the uniqueness structure and length are the same for both 'String'. Otherwise, they differs. This gives an opportunity to practically
-- and quickly synthesize differently sounding intervals. The main component of the sound includes the lower pure quint, which can be in
-- the same octave or in the one with the number lower by one. Please, check before executing 
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
uniqOverSoXSynth :: Float -> String -> IO ()
uniqOverSoXSynth x wws = do
  let note0 = closestNote x
      note1 = pureQuintNote note0
      v0    = uniqOvertonesV note0 wws
      v1    = uniqOvertonesV note1 wws
  _ <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "test-.wav", "synth", "0.5","sine", showFFloat (Just 4) note0 "",
     "synth", "0.5","sine", "mix", showFFloat (Just 4) note1 "", "vol","0.5"] ""
  uniqOverSoXSynthHelp v0
  uniqOverSoXSynthHelp2 v1
  mixTest

uniqOverSoXSynthHelp1 :: String -> OvertonesO -> IO ()
uniqOverSoXSynthHelp1 xs = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
 ["-r22050", "-n", xs ++ show (i + 2) ++ ".wav", "synth", "0.5","sine", showFFloat (Just 4) noteN "", "vol", showFFloat (Just 4) amplN ""] "") 

uniqOverSoXSynthHelp :: OvertonesO -> IO ()
uniqOverSoXSynthHelp = uniqOverSoXSynthHelp1 "test0"

uniqOverSoXSynthHelp2 :: OvertonesO -> IO ()
uniqOverSoXSynthHelp2 = uniqOverSoXSynthHelp1 "test1"

-- | For the given frequency and a Ukrainian text it generates a musical sound with the timbre obtained from the Ukrainian text (see the
-- documentation for @mmsyn7s@ package). The timbre for another given text usually differs, but can be the same. The last one is only
-- if the uniqueness structure and length are the same for both 'String'. Otherwise, they differs. This gives an opportunity to practically
-- and quickly synthesize differently sounding intervals. The main component of the sound includes the lower pure quint, which can be in
-- the same octave or in the one with the number lower by one. Please, check before executing 
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
-- The second 'String' argument is used to define signs for the harmonics coefficients for Overtones.
uniqOverSoXSynth2 :: Float -> String -> String -> IO ()
uniqOverSoXSynth2 x wws tts = do
  let note0 = closestNote x
      note1 = pureQuintNote note0
      v0    = uniqOvertonesV2 note0 wws tts
      v1    = uniqOvertonesV2 note1 wws tts
  _ <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "test-.wav", "synth", "0.5","sine", showFFloat (Just 4) note0 "", "synth",
     "0.5","sine", "mix", showFFloat (Just 4) note1 "", "vol","0.5"] ""
  uniqOverSoXSynthHelp v0
  uniqOverSoXSynthHelp2 v1
  mixTest

-- | Function to create a melody for the given arguments. The first 'String' is used to provide a rhythm. The second one -- to provide a timbre.
-- The timbre for another given text usually differs, but can be the same. This gives an opportunity to practically and quickly
-- synthesize differently sounding intervals. The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude for Overtones.
-- If it is set to 1.0 the overTones amplitudes are just maximum ones, otherwise they are multiplied by the parameter and this results in
-- their becoming more silent ones. The main component of the sound is in the given octave with a number given
-- by 'Int' parameter. Besides, another main component of the sound includes the lower pure quint, which can be in the same octave or in the one with
-- the number lower by one. The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
uniqOverSoXSynthN :: Int -> Float -> Float -> String -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN n ampL time3 zs = uniqOverSoXSynthN4G n ampL (str2DurationsDef n zs time3)

-- | Variant of the 'uniqOverSoXSynthN4G' function where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition.
uniqOverSoXSynthN4GS :: Int -> Float -> Float -> String -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN4GS n ampL time3 zs = uniqOverSoXSynthN4G n ampL (str2Durations zs time3)

-- | 4G generalized variant of the 'uniqOverSoXSynthN' where you specify your own 'Durations'.
uniqOverSoXSynthN4G :: Int -> Float -> Durations -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN4G n ampL v2 wws vec0
 | V.all (== 0.0) v2 = putStrLn "You provided no valid durations data! "
 | compare (abs ampL) 0.01 /= LT && compare (abs ampL) 1.0 /= GT =
    let v21 = V.filter (/= 0.0) v2
        m     = V.length v21
        zeroN = numVZeroesPre vec0 in V.imapM_ (\j x -> do   
          let note0 = closestNote x
              note1 = pureQuintNote note0
              v0    = uniqOvertonesV note0 wws
              v1    = uniqOvertonesV note1 wws
              uniqOverSoXSynthHelpN = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "test" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT 
                      then showFFloat (Just 4) (amplN * ampL) "" else "0"] "")
              uniqOverSoXSynthHelpN2 = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "testQ" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT 
                      then showFFloat (Just 4) (amplN * ampL) "" else "0"] "")
              soxSynthHelpMain note01 note02 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA" ++
                 prependZeroes zeroN "1" ++  ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note01 "", "synth", showFFloat (Just 4) (V.unsafeIndex v2 (j `rem` m)) "","sine", "mix", showFFloat (Just 4) note02 "", "vol",
                    if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then "0.5" else "0"]  ""
          _ <- soxSynthHelpMain note0 note1
          uniqOverSoXSynthHelpN v0
          uniqOverSoXSynthHelpN2 v1
          mixTest2 zeroN j) vec0
 | otherwise = let ampL1 = ampL - (fromIntegral . truncate $ ampL) in
    if abs ampL1 < 0.01 then uniqOverSoXSynthN4G n 0.01 v2 wws vec0
    else uniqOverSoXSynthN4G n ampL1 v2 wws vec0

-- | Function to create a melody for the given arguments. The first 'String' is used to provide a rhythm. The second one -- to provide a timbre.
-- The timbre for another given text usually differs, but can be the same. This gives an opportunity to practically and quickly
-- synthesize differently sounding intervals. The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude for Overtones.
-- If it is set to 1.0 the overTones amplitudes are just maximum ones, otherwise they are multiplied by the parameter and this results in
-- their becoming more silent ones. The main component of the sound is in the given octave with a number given
-- by 'Int' parameter. Besides, another main component of the sound includes the lower pure quint, which can be in the same octave or in the one with
-- the number lower by one. The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
-- The third 'String' argument is used to define signs of the harmonics coefficients in the generated sounds.
uniqOverSoXSynthN3 :: Int -> Float -> Float -> String -> String -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN3 n ampL time3 zs = uniqOverSoXSynthN34G n ampL (str2DurationsDef n zs time3)

-- | Variant of the 'uniqOverSoXSynthN34G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition.
uniqOverSoXSynthN34GS :: Int -> Float -> Float -> String -> String -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN34GS n ampL time3 zs = uniqOverSoXSynthN34G n ampL (str2Durations zs time3)

-- | 4G generalized variant of the 'uniqOverSoXSynthN3' where you specify your own 'Durations'. 
uniqOverSoXSynthN34G :: Int -> Float -> Durations -> String -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN34G n ampL v2 wws tts vec0
 | V.all (== 0.0) v2 = putStrLn "You provided no valid durations data! "
 | compare (abs ampL) 0.01 /= LT && compare (abs ampL) 1.0 /= GT =
    let v21 = V.filter (/=0.0) v2
        m     = V.length v2
        zeroN = numVZeroesPre vec0 in V.imapM_ (\j x -> do   
          let note0 = closestNote x                         -- zs ? vec0 -- are they related to the one object? No, they are obtained from different sources.
              note1 = pureQuintNote note0
              v0    = uniqOvertonesV2 note0 wws tts
              v1    = uniqOvertonesV2 note1 wws tts
              uniqOverSoXSynthHelpN vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "test" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") vec
              uniqOverSoXSynthHelpN2 vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "testQ" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") vec 
              soxSynthHelpMain note01 note02 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA" ++
                prependZeroes zeroN "1" ++  ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note01 "",
                  "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", "mix", showFFloat (Just 4) note02 "", "vol",
                    if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then "0.5" else "0"] ""
          _ <- soxSynthHelpMain note0 note1
          uniqOverSoXSynthHelpN v0
          uniqOverSoXSynthHelpN2 v1
          mixTest2 zeroN j) vec0
 | otherwise = let ampL1 = ampL - (fromIntegral . truncate $ ampL) in
    if abs ampL1 < 0.01 then uniqOverSoXSynthN34G n 0.01 v2 wws tts vec0
    else uniqOverSoXSynthN34G n ampL1 v2 wws tts vec0    

-- | Function to create a melody for the given arguments. The first 'String' is used to provide a rhythm. The second one -- to provide a timbre.
-- The timbre for another given text usually differs, but can be the same. This gives an opportunity to practically and quickly
-- synthesize differently sounding intervals. The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude for Overtones.
-- If it is set to 1.0 the overTones amplitudes are just maximum ones, otherwise they are multiplied by the parameter and this results in
-- their becoming more silent ones. The main component of the sound is in the given octave with a number given
-- by 'Int' parameter. Besides, another main component of the sound includes the lower pure quint, which can be in the same octave or in the one with
-- the number lower by one. The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
-- The third 'String' argument is used to define signs of the harmonics coefficients in the generated sounds.
-- The fourth 'String' argument is used to define the intervals for the notes if any.
-- The third 'Float' parameter basically is used to define in how many times the volume for the second lower note is less than the volume of
-- the main note. If it is rather great, it can signal that the volume for the second note overTones are greater than for the main note obetones.
-- The last one is experimental feature.
uniqOverSoXSynthN4 :: Int -> Float -> Float -> Float -> String -> String -> String -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN4 n ampL time3 dAmpl zs = uniqOverSoXSynthN44G n ampL dAmpl (str2DurationsDef n zs time3)

-- | Variant of the 'uniqOverSoXSynthN44G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition.
uniqOverSoXSynthN44GS :: Int -> Float -> Float -> Float -> String -> String -> String -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN44GS n ampL time3 dAmpl zs = uniqOverSoXSynthN44G n ampL dAmpl (str2Durations zs time3)

-- | 4G generalized variant of the 'uniqOverSoXSynthN4' where you specify your own 'Durations'. 
uniqOverSoXSynthN44G :: Int -> Float -> Float -> Durations -> String -> String -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN44G n ampL dAmpl v2 wws tts vs = uniqOverSoXSynthN45G n ampL dAmpl v2 wws tts (intervalsFromString vs)
 
-- | 5G generalized variant of the 'uniqOverSoXSynthN44G' where you specify your own 'Intervals'. 
uniqOverSoXSynthN45G :: Int -> Float -> Float -> Durations -> String -> String -> Intervals -> V.Vector Float -> IO ()
uniqOverSoXSynthN45G n ampL dAmpl v2 wws tts v3 vec0
 | V.all (== 0.0) v2 = putStrLn "You provided no valid durations data! "
 | compare (abs ampL) 0.01 /= LT && compare (abs ampL) 1.0 /= GT =
    let v21 = V.filter (/=0.0) v2
        m     = V.length v2
        zeroN = numVZeroesPre vec0
        l     = V.length v3 in V.imapM_ (\j x -> do   
          let note0 = closestNote x
              note1 = dNote (V.unsafeIndex v3 (j `rem` l)) note0
              v0    = uniqOvertonesV2 note0 wws tts
              v1    = if isNothing note1 then V.empty
                      else uniqOvertonesV2 (fromJust note1) wws tts
              uniqOverSoXSynthHelpN = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "test" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                  "sine",showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") 
              uniqOverSoXSynthHelpN2 = V.imapM_ (\i (noteN, amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "testQ" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (if dAmpl * amplN * ampL > 1.0 then 1.0
                      else dAmpl * amplN * ampL) "" else "0"] "") 
              soxSynthHelpMain0 note01 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA" ++
                 prependZeroes zeroN "1" ++  ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note01 "",
                   "vol",if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then "0.5" else "0"] ""
              soxSynthHelpMain1 note02 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testB" ++
                 prependZeroes zeroN "1" ++  ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note02 "",
                    "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (if dAmpl > 0.5 then 0.5 else dAmpl / 2) "" else "0"] ""
          if isNothing note1 then do { _ <- soxSynthHelpMain0 note0
                                     ; uniqOverSoXSynthHelpN v0 }
          else do { _ <- soxSynthHelpMain0 note0
                  ; _ <- soxSynthHelpMain1 (fromJust note1)
                  ; uniqOverSoXSynthHelpN v0
                  ; uniqOverSoXSynthHelpN2 v1}
          mixTest2 zeroN j) vec0
 | otherwise = let ampL1 = ampL - (fromIntegral . truncate $ ampL) in
    if abs ampL1 < 0.01 then uniqOverSoXSynthN45G n 0.01 dAmpl v2 wws tts v3 vec0
    else uniqOverSoXSynthN45G n ampL1 dAmpl v2 wws tts v3 vec0    

-- | 6G generalized variant of the 'uniqOverSoXSynthN45G' where you specify your own 'Strengths' and a limit (as the last 'Float') when less volume level 
-- sound files are treated as a silent ones and are not adjusted. 
uniqOverSoXSynthN46G :: Int -> Float -> Float -> Durations -> String -> String -> Intervals -> V.Vector Float -> Strengths -> Float -> IO ()
uniqOverSoXSynthN46G n ampL dAmpl v2 wws tts v3 vec0 v6 limV
 | V.all (== 0.0) v2 = putStrLn "You provided no valid durations data! "
 | V.null v6 = putStrLn "You did not provide a volume adjustments vector! "
 | compare (abs ampL) 0.01 /= LT && compare (abs ampL) 1.0 /= GT =
    let v21 = V.filter (/=0.0) v2
        m     = V.length v2
        zeroN = numVZeroesPre vec0
        l     = V.length v3 in V.imapM_ (\j x -> do   
          let note0 = closestNote x
              note1 = dNote (V.unsafeIndex v3 (j `rem` l)) note0
              v0    = uniqOvertonesV2 note0 wws tts
              v1    = if isNothing note1 then V.empty
                      else uniqOvertonesV2 (fromJust note1) wws tts
              uniqOverSoXSynthHelpN = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "test" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                  "sine",showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") 
              uniqOverSoXSynthHelpN2 = V.imapM_ (\i (noteN, amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "testQ" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (if dAmpl * amplN * ampL > 1.0 then 1.0
                      else dAmpl * amplN * ampL) "" else "0"] "") 
              soxSynthHelpMain0 note01 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA" ++
                 prependZeroes zeroN "1" ++  ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note01 "",
                   "vol",if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then "0.5" else "0"] ""
              soxSynthHelpMain1 note02 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testB" ++
                 prependZeroes zeroN "1" ++  ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note02 "",
                    "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (if dAmpl > 0.5 then 0.5 else dAmpl / 2) "" else "0"] ""
          if isNothing note1 then do { _ <- soxSynthHelpMain0 note0
                                     ; uniqOverSoXSynthHelpN v0 }
          else do { _ <- soxSynthHelpMain0 note0
                  ; _ <- soxSynthHelpMain1 (fromJust note1)
                  ; uniqOverSoXSynthHelpN v0
                  ; uniqOverSoXSynthHelpN2 v1}
          mixTest2 zeroN j
          apply6GSilentFile ("result" ++ prependZeroes zeroN (show j) ++ ".wav") limV (V.unsafeIndex v6 (j `rem` V.length v6))) vec0
 | otherwise = let ampL1 = ampL - (fromIntegral . truncate $ ampL) in
    if abs ampL1 < 0.01 then uniqOverSoXSynthN46G n 0.01 dAmpl v2 wws tts v3 vec0 v6 limV
    else uniqOverSoXSynthN46G n ampL1 dAmpl v2 wws tts v3 vec0 v6 limV

-- | Variant of the 'uniqOverSoXSynthN45G' where 'Intervals' are obtained from the 'String' using 'intervalsFromStringG' function. Helps to create a speech-like 
-- composition.
uniqOverSoXSynthN45GS :: Int -> Float -> Float -> Float -> String -> String -> String -> Intervals -> String -> V.Vector Float -> IO ()
uniqOverSoXSynthN45GS n ampL time3 dAmpl zs wws tts v3 vs = uniqOverSoXSynthN45G n ampL dAmpl (str2Durations zs time3) wws tts (intervalsFromStringG v3 vs) 

-- | Variant of the 'uniqOverSoXSynthN46G' where 'Strengths' are obtained from the 'String' using 'str2Volume' function. Helps to create a speech-like 
-- composition.
uniqOverSoXSynthN46GS :: Int -> Float -> Float -> Float -> String -> String -> String -> Intervals -> String -> V.Vector Float -> String ->
  Float -> IO ()
uniqOverSoXSynthN46GS n ampL time3 dAmpl zs wws tts v3 vs vec0 xxs limV = uniqOverSoXSynthN46G n ampL dAmpl (str2Durations zs time3) wws tts 
  (intervalsFromStringG v3 vs) vec0 (str2Volume xxs) limV

-- | A variant of 'uniqOverSoXSynthN46GS' where 'Strengths' and 'Durations' are obtained from the same Ukrainian text specified as 
-- the last 'String' argument. The second 'Float' argument is an average duration of the sounds in seconds. 
-- Note that 'Int' arguments are used by 'liftInEnku' in that order so it returns a 'Maybe' number (actually frequency) for 
-- the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthN46GSu :: Int -> Float -> Float -> Float -> String -> String -> Intervals -> String -> V.Vector Float -> String -> Float -> IO ()
uniqOverSoXSynthN46GSu n ampL time3 dAmpl wws tts v5 vs vec0 xxs limV = 
 uniqOverSoXSynthN46G n ampL dAmpl (str2Durations xxs time3) wws tts (intervalsFromStringG v5 vs) vec0 (str2Volume xxs) limV

-- | Similar to 'uniqOverSoXSynthN', but uses a sound file to obtain the information analogous to 'V.Vector' in the latter one. 
-- Besides, the function lifts the frequencies to the octave with the given by 'Int' parameter number (better to use from the range [1..8]).
-- The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude for Overtones. If it is set to 1.0 the
-- overTones amplitudes are just the maximum ones, otherwise they are multiplied by the parameter and this results in their becoming more silent ones.
-- The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" and \"end.wav\" files in the current directory, because they can be overwritten.
--
-- For better usage the 'FilePath' should be a filepath for the .wav file.
uniqOverSoXSynthNGen :: FilePath -> Int -> Float -> Float -> String -> String -> IO ()
uniqOverSoXSynthNGen file m = uniqOverSoXSynthNGenE file m 12

-- | Similar to 'uniqOverSoXSynthNGen', but uses additional second 'Int' parameter. It defines, to which n-th elements set (see 'nkyT') belongs the obtained
-- higher notes in the intervals. If that parameter equals to 12, then the function is practically equivalent to 'uniqOverSoXSynthNGen'. To obtain
-- its modifications, please, use 2, 3, 4, 6, or 9. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGenE :: FilePath -> Int -> Int -> Float -> Float -> String -> String -> IO ()
uniqOverSoXSynthNGenE file m ku ampL time3 zs wws = do
  n <- duration1000 file
  unGenNE4Gi n file m ku ampL (str2DurationsDef n zs time3) wws

-- | Generalized version of the 'uniqOverSoXSynthNGenE' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGenEPar :: FilePath -> Params -> Float -> Float -> String -> String -> IO ()
uniqOverSoXSynthNGenEPar file params ampL time3 zs wws = do
  n <- duration1000 file
  unGenNE4GiPar n file params ampL (str2DurationsDef n zs time3) wws  

-- | Variant of the 'uniqOverSoXSynthNGenE4G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGenE4GS :: FilePath -> Int -> Int -> Float -> Float -> String -> String -> IO ()
uniqOverSoXSynthNGenE4GS file m ku ampL time3 zs wws = do
  n <- duration1000 file
  unGenNE4Gi n file m ku ampL (str2Durations zs time3) wws
  
-- | Generalized version of the 'uniqOverSoXSynthNGenE4GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGenE4GSPar :: FilePath -> Params -> Float -> Float -> String -> String -> IO ()
uniqOverSoXSynthNGenE4GSPar file params ampL time3 zs wws = do
  n <- duration1000 file
  unGenNE4GiPar n file params ampL (str2Durations zs time3) wws  

-- | Note that the last two 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The third 'Int' parameter defines that @n@.
unGenNE4Gi :: Int -> FilePath -> Int -> Int -> Float -> Durations -> String -> IO ()
unGenNE4Gi n file m ku ampL v2 wws = do
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
  uniqOverSoXSynthN4G n ampL v2 wws vecB
  endFromResult

-- | Generalized version of the 'unGenNE4Gi' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
unGenNE4GiPar :: Int -> FilePath -> Params -> Float -> Durations -> String -> IO ()
unGenNE4GiPar n file params ampL v2 wws = do
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
  uniqOverSoXSynthN4G n ampL v2 wws vecB
  endFromResult  

-- | 4G genaralized version of the 'uniqOverSoXSynthNGenE' where you provide your own 'Durations'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGenE4G :: FilePath -> Int -> Int -> Float -> Durations -> String -> IO ()
uniqOverSoXSynthNGenE4G file m ku ampL v2 wws = do
  n <- duration1000 file
  unGenNE4Gi n file m ku ampL v2 wws

-- | Generalized version of the 'uniqOverSoXSynthNGenE4G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGenE4GPar :: FilePath -> Params -> Float -> Durations -> String -> IO ()
uniqOverSoXSynthNGenE4GPar file params ampL v2 wws = do
  n <- duration1000 file
  unGenNE4GiPar n file params ampL v2 wws   

-- | Similar to 'uniqOverSoXSynthN', but uses a sound file to obtain the information analogous to 'V.Vector' in the latter one. 
-- Besides, the function lifts the frequencies to the octave with the given by 'Int' parameter number (better to use from the range [1..8]).
-- The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude for Overtones. If it is set to 1.0 the
-- overTones amplitudes are just the maximum ones, otherwise they are multiplied by the parameter and this results in their becoming more silent ones.
-- The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" and \"end.wav\" files in the current directory, because they can be overwritten.
--
-- For better usage the 'FilePath' should be a filepath for the .wav file.
-- The third 'String' argument is used to define signs of the harmonics coefficients in the generated sounds.
uniqOverSoXSynthNGen3 :: FilePath -> Int -> Float -> Float -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen3 file m = uniqOverSoXSynthNGen3E file m 12

-- | Similar to 'uniqOverSoXSynthNGen3', but uses additional second 'Int' parameter. It defines, to which n-th elements set (see 'nkyT') belongs the obtained
-- higher notes in the intervals. If that parameter equals to 12, then the function is practically equivalent to 'uniqOverSoXSynthNGen3'. To obtain
-- its modifications, please, use 2, 3, 4, 6, or 9. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen3E :: FilePath -> Int -> Int -> Float -> Float -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen3E file m ku ampL time3 zs wws tts = do
  n <- duration1000 file
  unGenN3E4Gi n file m ku ampL (str2DurationsDef n zs time3) wws tts

-- | Generalized version of the 'uniqOverSoXSynthNGen3E' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen3EPar :: FilePath -> Params -> Float -> Float -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen3EPar file params ampL time3 zs wws tts = do
  n <- duration1000 file
  unGenN3E4GiPar n file params ampL (str2DurationsDef n zs time3) wws tts  

-- | Variant of the 'uniqOverSoXSynthNGen3E4G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen3E4GS :: FilePath -> Int -> Int -> Float -> Float -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen3E4GS file m ku ampL time3 zs wws tts = do
  n <- duration1000 file
  unGenN3E4Gi n file m ku ampL (str2Durations zs time3) wws tts

-- | Generalized version of the 'uniqOverSoXSynthNGen3E4GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen3E4GSPar :: FilePath -> Params -> Float -> Float -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen3E4GSPar file params ampL time3 zs wws tts = do
  n <- duration1000 file
  unGenN3E4GiPar n file params ampL (str2Durations zs time3) wws tts

-- | Note that the last two 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The third 'Int' parameter defines that @n@.
unGenN3E4Gi :: Int -> FilePath -> Int -> Int -> Float -> Durations -> String -> String -> IO ()
unGenN3E4Gi n file m ku ampL v2 wws tts = do
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
  uniqOverSoXSynthN34G n ampL v2 wws tts vecB
  endFromResult

-- | Generalized version of the 'unGenN3E4Gi' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
unGenN3E4GiPar :: Int -> FilePath -> Params -> Float -> Durations -> String -> String -> IO ()
unGenN3E4GiPar n file params ampL v2 wws tts = do
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
  uniqOverSoXSynthN34G n ampL v2 wws tts vecB
  endFromResult  

-- | 4G genaralized version of the 'uniqOverSoXSynthNGen3E' where you provide your own 'Durations'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen3E4G :: FilePath -> Int -> Int -> Float -> Durations -> String -> String -> IO ()
uniqOverSoXSynthNGen3E4G file m ku ampL v2 wws tts = do
  n <- duration1000 file
  unGenN3E4Gi n file m ku ampL v2 wws tts
   
-- | Generalized version of the 'uniqOverSoXSynthNGen3E4G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen3E4GPar :: FilePath -> Params -> Float -> Durations -> String -> String -> IO ()
uniqOverSoXSynthNGen3E4GPar file params ampL v2 wws tts = do
  n <- duration1000 file
  unGenN3E4GiPar n file params ampL v2 wws tts   

-- | Similar to 'uniqOverSoXSynthN', but uses a sound file to obtain the information analogous to 'V.Vector' in the latter one. 
-- Besides, the function lifts the frequencies to the octave with the given by 'Int' parameter number (better to use from the range [1..8]).
-- The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude for Overtones. If it is set to 1.0 the
-- overTones amplitudes are just the maximum ones, otherwise they are multiplied by the parameter and this results in their becoming more silent ones.
-- The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" and \"end.wav\" files in the current directory, because they can be overwritten.
--
-- For better usage the 'FilePath' should be a filepath for the .wav file.
-- The third 'String' argument is used to define signs of the harmonics coefficients in the generated sounds.
-- The fourth 'String' argument is used to define the intervals for the notes if any.
-- The third 'Float' parameter basically is used to define in how many times the volume for the second lower note is less than the volume of
-- the main note. If it is rather great, it can signal that the volume for the second note overTones are greater than for the main note obetones.
-- The last one is an experimental feature.
uniqOverSoXSynthNGen4 :: FilePath -> Int -> Float -> Float -> Float -> String -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen4 file m = uniqOverSoXSynthNGen4E file m 12

-- | Similar to 'uniqOverSoXSynthNGen4', but uses additional second 'Int' parameter. It defines, to which n-th elements set (see 'nkyT') belongs the obtained
-- higher notes in the intervals. If that parameter equals to 12, then the function is practically equivalent to 'uniqOverSoXSynthNGen4'. To obtain
-- its modifications, please, use 2, 3, 4, 6, or 9. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen4E :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen4E file m ku ampL time3 dAmpl zs wws tts vs = do
  n <- duration1000 file
  unGenN4E4Gi n file m ku ampL dAmpl (str2DurationsDef n zs time3) wws tts vs

-- | Generalized version of the 'uniqOverSoXSynthNGen4E' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen4EPar :: FilePath -> Params -> Float -> Float -> Float -> String -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen4EPar file params ampL time3 dAmpl zs wws tts vs = do
  n <- duration1000 file
  unGenN4E4GiPar n file params ampL dAmpl (str2DurationsDef n zs time3) wws tts vs  

-- | Variant of the 'uniqOverSoXSynthNGen4E4G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen4E4GS :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen4E4GS file m ku ampL time3 dAmpl zs wws tts vs = do
  n <- duration1000 file
  unGenN4E4Gi n file m ku ampL dAmpl (str2Durations zs time3) wws tts vs

-- | Generalized version of the 'uniqOverSoXSynthNGen4E4GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen4E4GSPar :: FilePath -> Params -> Float -> Float -> Float -> String -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen4E4GSPar file params ampL time3 dAmpl zs wws tts vs = do
  n <- duration1000 file
  unGenN4E4GiPar n file params ampL dAmpl (str2Durations zs time3) wws tts vs  

-- | Note that the last two 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The third 'Int' parameter defines that @n@.
unGenN4E4Gi :: Int -> FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> String -> String -> IO ()
unGenN4E4Gi n file m ku ampL dAmpl v2 wws tts vs = do
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
  uniqOverSoXSynthN44G n ampL dAmpl v2 wws tts vs vecB
  endFromResult

-- | Generalized version of the 'unGenN4E4Gi' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
unGenN4E4GiPar :: Int -> FilePath -> Params -> Float -> Float -> Durations -> String -> String -> String -> IO ()
unGenN4E4GiPar n file params ampL dAmpl v2 wws tts vs = do
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
  uniqOverSoXSynthN44G n ampL dAmpl v2 wws tts vs vecB
  endFromResult  

-- | Note that the last two 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The third 'Int' parameter defines that @n@.
unGenN4E5Gi :: Int -> FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> String -> Intervals -> IO ()
unGenN4E5Gi n file m ku ampL dAmpl v2 wws tts v3 = do
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
  uniqOverSoXSynthN45G n ampL dAmpl v2 wws tts v3 vecB
  endFromResult

-- | Generalized version of the 'unGenN4E5Gi' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
unGenN4E5GiPar :: Int -> FilePath -> Params -> Float -> Float -> Durations -> String -> String -> Intervals -> IO ()
unGenN4E5GiPar n file params ampL dAmpl v2 wws tts v3 = do
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
  uniqOverSoXSynthN45G n ampL dAmpl v2 wws tts v3 vecB
  endFromResult  
  
-- | 4G genaralized version of the 'uniqOverSoXSynthNGen4E' where you provide your own 'Durations'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen4E4G :: FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen4E4G file m ku ampL dAmpl v2 wws tts vs = do
  n <- duration1000 file
  unGenN4E4Gi n file m ku ampL dAmpl v2 wws tts vs

-- | Generalized version of the 'uniqOverSoXSynthNGen4E4G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen4E4GPar :: FilePath -> Params -> Float -> Float -> Durations -> String -> String -> String -> IO ()
uniqOverSoXSynthNGen4E4GPar file params ampL dAmpl v2 wws tts vs = do
  n <- duration1000 file
  unGenN4E4GiPar n file params ampL dAmpl v2 wws tts vs

-- | 5G genaralized version of the 'uniqOverSoXSynthNGen4E' where you provide your own 'Durations' and 'Intervals'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen4E5G :: FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> String -> Intervals -> IO ()
uniqOverSoXSynthNGen4E5G file m ku ampL dAmpl v2 wws tts v3 = do
  n <- duration1000 file
  unGenN4E5Gi n file m ku ampL dAmpl v2 wws tts v3

-- | Generalized version of the 'uniqOverSoXSynthNGen4E5G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen4E5GPar :: FilePath -> Params -> Float -> Float -> Durations -> String -> String -> Intervals -> IO ()
uniqOverSoXSynthNGen4E5GPar file params ampL dAmpl v2 wws tts v3 = do
  n <- duration1000 file
  unGenN4E5GiPar n file params ampL dAmpl v2 wws tts v3  

-- | Variant of the 'uniqOverSoXSynthNGen4E5G' where 'Intervals' are obtained from the 'String' using 'intervalsFromStringG' function. Helps to create a speech-like 
-- composition. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen4E5GS :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> String -> String -> Intervals -> String -> IO ()
uniqOverSoXSynthNGen4E5GS file m ku ampL time3 dAmpl zs wws tts v3 vs = do
  n <- duration1000 file
  unGenN4E5Gi n file m ku ampL dAmpl (str2Durations zs time3) wws tts (intervalsFromStringG v3 vs)

-- | Generalized version of the 'uniqOverSoXSynthNGen4E5GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen4E5GSPar :: FilePath -> Params -> Float -> Float -> Float -> String -> String -> String -> Intervals -> String -> IO ()
uniqOverSoXSynthNGen4E5GSPar file params ampL time3 dAmpl zs wws tts v3 vs = do
  n <- duration1000 file
  unGenN4E5GiPar n file params ampL dAmpl (str2Durations zs time3) wws tts (intervalsFromStringG v3 vs)  

-- | 6G generalized function for 'uniqOverSoXSynthNGen4E5G' where you provide your own 'Strengths'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen4E6G :: FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> String -> Intervals -> Strengths -> Float -> IO ()
uniqOverSoXSynthNGen4E6G file m ku ampL dAmpl v2 wws tts v3 v6 limV = 
 uniqOverSoXSynthNGen4E5G file m ku ampL dAmpl v2 wws tts v3 >> apply6G2 v6 "221w" "result" limV >> endFromResult

-- | Generalized version of the 'uniqOverSoXSynthNGen4E6G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen4E6GPar :: FilePath -> Params -> Float -> Float -> Durations -> String -> String -> Intervals -> Strengths -> Float -> IO ()
uniqOverSoXSynthNGen4E6GPar file params ampL dAmpl v2 wws tts v3 v6 limV = 
 uniqOverSoXSynthNGen4E5GPar file params ampL dAmpl v2 wws tts v3 >> apply6G2 v6 "221w" "result" limV >> endFromResult 

-- | A variant of 'uniqOverSoXSynthNGen4E6G' where 'Strengths' are obtained from a Ukrainian text specified as the last 'String' argument. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen4E6GS :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> String -> String -> Intervals -> String -> 
  String -> Float -> IO ()
uniqOverSoXSynthNGen4E6GS file m ku ampL time3 dAmpl zs wws tts v5 vs xxs limV = 
 uniqOverSoXSynthNGen4E6G file m ku ampL dAmpl (str2Durations zs time3) wws tts (intervalsFromStringG v5 vs) (str2Volume xxs) limV

-- | Generalized version of the 'uniqOverSoXSynthNGen4E6GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen4E6GSPar :: FilePath -> Params -> Float -> Float -> Float -> String -> String -> String -> Intervals -> String -> 
  String -> Float -> IO ()
uniqOverSoXSynthNGen4E6GSPar file params ampL time3 dAmpl zs wws tts v5 vs xxs limV = 
 uniqOverSoXSynthNGen4E6GPar file params ampL dAmpl (str2Durations zs time3) wws tts (intervalsFromStringG v5 vs) (str2Volume xxs) limV 
 
-- | A variant of 'uniqOverSoXSynthNGen4E6GS' where 'Strengths' and 'Durations' are obtained from the same Ukrainian text specified as 
-- the last 'String' argument. The second 'Float' argument is an average duration of the sounds in seconds. 
-- Note that 'Int' arguments are used by 'liftInEnku' in that order so it returns a 'Maybe' number (actually frequency) for 
-- the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
uniqOverSoXSynthNGen4E6GSu :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> String -> Intervals -> String -> String -> Float -> IO ()
uniqOverSoXSynthNGen4E6GSu file m ku ampL time3 dAmpl wws tts v5 vs xxs limV = 
 uniqOverSoXSynthNGen4E6G file m ku ampL dAmpl (str2Durations xxs time3) wws tts (intervalsFromStringG v5 vs) (str2Volume xxs) limV
 
-- | Generalized version of the 'uniqOverSoXSynthNGen4E6GSu' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
uniqOverSoXSynthNGen4E6GSuPar :: FilePath -> Params -> Float -> Float -> Float -> String -> String -> Intervals -> String -> String -> Float -> IO ()
uniqOverSoXSynthNGen4E6GSuPar file params ampL time3 dAmpl wws tts v5 vs xxs limV = 
 uniqOverSoXSynthNGen4E6GPar file params ampL dAmpl (str2Durations xxs time3) wws tts (intervalsFromStringG v5 vs) (str2Volume xxs) limV

