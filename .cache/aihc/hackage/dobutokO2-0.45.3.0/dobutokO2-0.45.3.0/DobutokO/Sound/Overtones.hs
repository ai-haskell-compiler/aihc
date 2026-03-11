-- |
-- Module      :  DobutokO.Sound.Overtones
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Overtones (
  -- * Library and executable functions
  -- ** For the fixed timbre
  overSoXSynthN
  -- *** For the fixed timbre with different signs for harmonics coefficients
  , overTones2
  , overSoXSynth2
  , overSoXSynthN2
  , overSoXSynthN3
  -- *** Use additional parameters
  , overSoXSynthDN
  , overSoXSynth2DN
  -- *** Use a file for information
  , overSoXSynthNGen
  , overSoXSynthNGen2
  , overSoXSynthNGen3
  -- * Extended generation using enky functionality
  -- ** With somewhat fixed timbre
  , overSoXSynthNGenE
  , overSoXSynthNGen2E
  , overSoXSynthNGen3E
  -- * New 4G functions to work with Durations
  , overSoXSynthN4G
  , overSoXSynthN24G
  , overSoXSynthN34G
  , overSoXSynthNGenE4G
  , overSoXSynthNGen2E4G
  , overSoXSynthNGen3E4G
  -- ** 4G with speech-like composition
  , overSoXSynthN4GS
  , overSoXSynthN24GS
  , overSoXSynthN34GS
  , overSoXSynthNGenE4GS
  , overSoXSynthNGen2E4GS
  , overSoXSynthNGen3E4GS
  -- * New 5G functions to work also with Intervals
  , overSoXSynthN35G
  , overSoXSynthNGen3E5G
  -- ** 5G with obtained from the text arbitraty length Intervals
  , overSoXSynthN35GS
  , overSoXSynthNGen3E5GS
  -- * New 6G function to work also with Strengths
  , overSoXSynthNGen3E6G
  -- ** 6G with obtained from the text arbitrary length Strengths
  , overSoXSynthNGen3E6GS
  , overSoXSynthNGen3E6GSu
  -- * New generalized functions working with Params
  , overSoXSynthNGenEPar
  , overSoXSynthNGenE4GSPar
  , overSoXSynthNGenE4GPar
  , overSoXSynthNGen2EPar
  , overSoXSynthNGen2E4GSPar
  , overSoXSynthNGen2E4GPar
  , overSoXSynthNGen3EPar
  , overSoXSynthNGen3E4GSPar
  , overSoXSynthNGen3E4GPar
  , overSoXSynthNGen3E5GPar
  , overSoXSynthNGen3E5GSPar
  , overSoXSynthNGen3E6GPar
  , overSoXSynthNGen3E6GSPar
  , overSoXSynthNGen3E6GSuPar
) where

import Numeric (showFFloat)
import Data.List (isPrefixOf,sort)
import Data.Maybe (isNothing,fromJust,maybe)
import qualified Data.Vector as V
import System.Process
import EndOfExe2 (showE)
import System.Directory
import Aftovolio.Ukrainian.Melodics (convertToProperUkrainianI8)
import DobutokO.Sound.Functional.Basics
import DobutokO.Sound.Functional.Params
import DobutokO.Sound.DIS5G6G

-- | For the given frequency of the note and a Ukrainian text it generates a 'V.Vector' of the tuples, each one of which contains
-- the harmonics' frequency and amplitude. The 'String' is used to produce the signs for harmonics coefficients.
overTones2 :: Float -> String -> OvertonesO
overTones2 note ts =
  V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) . V.filter (\(_, t4) -> t4 /= 0.0) .
    V.zip (V.generate 1024 (\i -> note * fromIntegral (i + 2))) $ (V.generate 1024 (\i -> fromIntegral (V.unsafeIndex (signsFromString 1024 ts)
      (i + 1)) / fromIntegral ((i + 1) * (i + 1))))

-- | Similar to 'overSoXSynth' except that takes not necessarily pure lower quint note as the second one, but the one specified by the 'String' parameter
-- as an argument to 'dNote'. If you begin the 'String' with space characters, or \"сь\", or \"ць\", or dash, or apostrophe, or soft sign, than there will
-- be no interval and the sound will be solely one with its 'OvertonesO'.
overSoXSynthDN :: Float -> String -> IO ()
overSoXSynthDN x = overSoXSynth2DN x 0.5

-- | Similar to 'overSoXSynthDN' except that the resulting duration is specified by the second 'Float' parameter in seconds. For 'overSoXSynthDN'
-- it is equal to 0.5.
overSoXSynth2DN :: Float -> Float -> String -> IO ()
overSoXSynth2DN x y zs
 | null . convertToProperUkrainianI8 $ zs = overSoXSynth x
 | otherwise = do
    let note0 = closestNote x
        note1 = dNote (V.unsafeIndex (intervalsFromString zs) 0) note0
        v0    = overTones note0
        v1    = maybe V.empty overTones note1
        overSoXSynthHelp vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
          ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", showFFloat (Just 4) y "","sine", showFFloat (Just 4) noteN "",
            "vol", showFFloat (Just 4) amplN ""] "") vec
        overSoXSynthHelp2 vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
          ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", showFFloat (Just 4) y "","sine", showFFloat (Just 4) noteN "",
            "vol", showFFloat (Just 4) amplN ""] "") vec
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA.wav", "synth", showFFloat (Just 4) y "","sine",
       showFFloat (Just 4) note0 "", "vol","0.5"] ""
    if isNothing note1 then overSoXSynthHelp v0
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testB.wav", "synth", showFFloat (Just 4) y "","sine",
         showFFloat (Just 4) (fromJust note1) "", "vol","0.5"] ""
      overSoXSynthHelp v0
      overSoXSynthHelp2 v1
    mixTest

-- | For the given frequency it generates a musical sound with a timbre. The main component of the sound includes the lower pure quint,
-- which can be in the same octave or in the one with the number lower by one. Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
-- The 'String' argument is used to define signs of the harmonics coefficients for Overtones.
overSoXSynth2 :: Float -> String -> IO ()
overSoXSynth2 x tts = do
  let note0 = closestNote x
      note1 = pureQuintNote note0
      v0    = overTones2 note0 tts
      v1    = overTones2 note1 tts
      overSoXSynthHelp vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
        ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", "0.5","sine", showFFloat (Just 4) noteN "",
           "vol", showFFloat (Just 4) amplN ""] "") vec
      overSoXSynthHelp2 vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
        ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", "0.5","sine", showFFloat (Just 4) noteN "",
           "vol", showFFloat (Just 4) amplN ""] "") vec
  _ <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "test01.wav", "synth", "0.5","sine", showFFloat (Just 4) note0 "",
     "synth", "0.5","sine", "mix", showFFloat (Just 4) note1 "", "vol","0.5"] ""
  overSoXSynthHelp v0
  overSoXSynthHelp2 v1
  mixTest 

-- | Function to create a melody for the given arguments. 'String' is used to provide a rhythm. The main component of the sound includes the lower pure quint, which
-- can be in the same octave or in the one with the number lower by one. The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude
-- for Overtones. If it is set to 1.0 the overTones amplitudes are just the maximum ones, otherwise they are multiplied by the parameter and this results
-- in their becoming more silent ones. The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
overSoXSynthN :: Int -> Float -> Float -> String -> V.Vector Float -> IO ()
overSoXSynthN n ampL time3 zs = overSoXSynthN4G n ampL (str2DurationsDef n zs time3)

-- | Function to create a melody for the given arguments. 'Durations' is used to provide a rhythm. 
overSoXSynthN4G :: Int -> Float -> Durations -> V.Vector Float -> IO ()
overSoXSynthN4G n ampL v2 vec0
 | V.all (== 0.0) v2 = putStrLn "You provided no valid durations data! "
 | compare (abs ampL) 0.01 /= LT && compare (abs ampL) 1.0 /= GT = 
    let zeroN = numVZeroesPre vec0
        v21 = V.filter (/=0.0) v2
        m = V.length v2 in V.imapM_ (\j x -> do 
          let note0 = closestNote x                     -- zs is obtained from the command line arguments
              note1 = pureQuintNote note0
              v0    = overTones note0
              v1    = overTones note1
              overSoXSynthHelpN vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "test" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") vec
              overSoXSynthHelpN2 vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "testQ" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") vec  
              soxSynthHelpMain note01 note02 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA" ++ prependZeroes zeroN "1" ++
                ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note01 "", "synth",
                  showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", "mix", showFFloat (Just 4) note02 "", "vol",if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT 
                    then "0.5" else "0"] ""
          _ <- soxSynthHelpMain note0 note1
          overSoXSynthHelpN v0
          overSoXSynthHelpN2 v1
          mixTest2 zeroN j) vec0
 | otherwise = let ampL1 = ampL - (fromIntegral . truncate $ ampL) in
    if abs ampL1 < 0.01 then overSoXSynthN4G n 0.01 v2 vec0
    else overSoXSynthN4G n ampL1 v2 vec0

-- | Variant of the 'overSoXSynthN4G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition.
overSoXSynthN4GS :: Int -> Float -> Float -> String -> V.Vector Float -> IO ()
overSoXSynthN4GS n ampL time3 zs = overSoXSynthN4G n ampL (str2Durations zs time3)

-- | Function to create a melody for the given arguments. 'String' is used to provide a rhythm. The main component of the sound includes the lower pure quint, which
-- can be in the same octave or in the one with the number lower by one. The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude
-- for Overtones. If it is set to 1.0 the overTones amplitudes are just the maximum ones, otherwise they are multiplied by the parameter and this results
-- in their becoming more silent ones. The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
overSoXSynthN2 :: Int -> Float -> Float -> String -> String -> V.Vector Float -> IO ()
overSoXSynthN2 n ampL time3 zs = overSoXSynthN24G n ampL (str2DurationsDef n zs time3)
 
-- | Function to create a melody for the given arguments. 'Durations' is used to provide a rhythm. 
overSoXSynthN24G :: Int -> Float -> Durations -> String -> V.Vector Float -> IO ()
overSoXSynthN24G n ampL v2 tts vec0
 | V.all (== 0.0) v2 = putStrLn "You provided no valid durations data! "
 | compare (abs ampL) 0.01 /= LT && compare (abs ampL) 1.0 /= GT = 
    let v21 = V.filter (/= 0.0) v2
        zeroN = numVZeroesPre vec0
        m = V.length v21 in V.imapM_ (\j x -> do 
          let note0 = closestNote x                     -- zs is obtained from the command line arguments
              note1 = pureQuintNote note0
              v0    = overTones2 note0 tts
              v1    = overTones2 note1 tts
              overSoXSynthHelpN vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "test" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                  "sine",showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") vec
              overSoXSynthHelpN2 vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "testQ" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") vec  
              soxSynthHelpMain note01 note02 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA" ++ prependZeroes zeroN "1" ++
                ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note01 "",
                  "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", "mix", showFFloat (Just 4) note02 "", "vol",
                     if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then "0.5" else "0"] ""
          _ <- soxSynthHelpMain note0 note1
          overSoXSynthHelpN v0
          overSoXSynthHelpN2 v1
          mixTest2 zeroN j) vec0
 | otherwise = let ampL1 = ampL - (fromIntegral . truncate $ ampL) in
    if abs ampL1 < 0.01 then overSoXSynthN24G n 0.01 v2 tts vec0
    else overSoXSynthN24G n ampL1 v2 tts vec0        

-- | Variant of the 'overSoXSynthN24G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition.
overSoXSynthN24GS :: Int -> Float -> Float -> String -> String -> V.Vector Float -> IO ()
overSoXSynthN24GS n ampL time3 zs = overSoXSynthN24G n ampL (str2Durations zs time3)

-- | Function to create a melody for the given arguments. 'String' is used to provide a rhythm. The main component of the sound includes the lower pure quint, which
-- can be in the same octave or in the one with the number lower by one. The first 'Float' argument from the range [0.01..1.0] is used as a maximum amplitude
-- for Overtones. If it is set to 1.0 the overTones amplitudes are just the maximum ones, otherwise they are multiplied by the parameter and this results
-- in their becoming more silent ones. The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" files in the current directory, because they can be overwritten.
-- The third 'String' argument is used to define the intervals for the notes if any.
-- The third 'Float' parameter basically is used to define in how many times the volume for the second lower note is less than the volume of
-- the main note. If it is rather great, it can signal that the volume for the second note overTones are greater than for the main note obetones.
-- The last one is experimental feature.
overSoXSynthN3 :: Int -> Float -> Float -> Float -> String -> String -> String -> V.Vector Float -> IO ()
overSoXSynthN3 n ampL time3 dAmpl zs = overSoXSynthN34G n ampL dAmpl (str2DurationsDef n zs time3)

-- | Function to create a melody for the given arguments. 'Duraitons' is used to provide a rhythm. 
overSoXSynthN34G :: Int -> Float -> Float -> Durations -> String -> String -> V.Vector Float -> IO ()
overSoXSynthN34G n ampL dAmpl v2 tts vs vec0
 | V.all (== 0.0) v2 = putStrLn "You provided no valid durations data! "
 | compare (abs ampL) 0.01 /= LT && compare (abs ampL) 1.0 /= GT = 
    let v21 = V.filter (/= 0.0) v2
        m     = V.length v21
        zeroN = numVZeroesPre vec0
        v3    = intervalsFromString vs
        l     = length vs in V.imapM_ (\j x -> do 
          let note0 = closestNote x                     -- zs is obtained from the command line arguments
              note1 = dNote (V.unsafeIndex v3 (j `rem` l)) note0
              v0    = overTones2 note0 tts
              v1    = if isNothing note1 then V.empty
                      else overTones2 (fromJust note1) tts
              overSoXSynthHelpN vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "test" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                  "sine",showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") vec
              overSoXSynthHelpN2 vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "testQ" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (if dAmpl * amplN * ampL > 1.0 then 1.0
                     else dAmpl * amplN * ampL) "" else "0"] "") vec  
              soxSynthHelpMain0 note01 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA" ++ prependZeroes zeroN "1" ++  ".wav",
                "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note01 "", "vol",
                  if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then "0.5" else "0"] ""
              soxSynthHelpMain1 note02 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testB" ++
                 prependZeroes zeroN "1" ++  ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note02 "",
                   "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (if dAmpl > 0.5 then 0.5 else dAmpl / 2) "" else "0"] ""
          if isNothing note1 then do { _ <- soxSynthHelpMain0 note0
                                     ; overSoXSynthHelpN v0 }
          else do { _ <- soxSynthHelpMain0 note0
                  ; _ <- soxSynthHelpMain1 (fromJust note1)
                  ; overSoXSynthHelpN v0
                  ; overSoXSynthHelpN2 v1}
          paths0 <- listDirectory "."
          let paths = sort . filter (isPrefixOf "test") $ paths0
          _ <- readProcessWithExitCode (fromJust (showE "sox")) (["--combine", "mix"] ++ paths ++ ["result0" ++ prependZeroes zeroN (show j) ++ ".wav","vol","0.3"]) ""
          mapM_ removeFile paths) vec0
 | otherwise = let ampL1 = ampL - (fromIntegral . truncate $ ampL) in
    if abs ampL1 < 0.01 then overSoXSynthN34G n 0.01 dAmpl v2 tts vs vec0
    else overSoXSynthN34G n ampL1 dAmpl v2 tts vs vec0

-- | Generalized variant of the 'overSoXSynthN34G' where you specify your own 'Intervals'. For more information, please, refer to 'intervalsFromStringG'.
overSoXSynthN35G :: Int -> Float -> Float -> Durations -> String -> Intervals -> V.Vector Float -> IO ()
overSoXSynthN35G n ampL dAmpl v2 tts v3 vec0
 | V.all (== 0.0) v2 = putStrLn "You provided no valid durations data! "
 | compare (abs ampL) 0.01 /= LT && compare (abs ampL) 1.0 /= GT = 
    let v21 = V.filter (/=0.0) v2
        m     = V.length v21
        zeroN = numVZeroesPre vec0
        l     = V.length v3 in V.imapM_ (\j x -> do 
          let note0 = closestNote x                     -- zs is obtained from the command line arguments
              note1 = dNote (V.unsafeIndex v3 (j `rem` l)) note0
              v0    = overTones2 note0 tts
              v1    = if isNothing note1 then V.empty
                      else overTones2 (fromJust note1) tts
              overSoXSynthHelpN vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "test" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                  "sine",showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (amplN * ampL) "" else "0"] "") vec
              overSoXSynthHelpN2 vec = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
                ["-r22050", "-n", "testQ" ++ prependZeroes zeroN (show (i + 2)) ++ ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "",
                   "sine", showFFloat (Just 4) noteN "", "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (if dAmpl * amplN * ampL > 1.0 then 1.0
                     else dAmpl * amplN * ampL) "" else "0"] "") vec  
              soxSynthHelpMain0 note01 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA" ++ prependZeroes zeroN "1" ++  ".wav",
                "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note01 "", "vol",
                  if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then "0.5" else "0"] ""
              soxSynthHelpMain1 note02 = readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testB" ++
                 prependZeroes zeroN "1" ++  ".wav", "synth", showFFloat (Just 4) (abs (V.unsafeIndex v21 (j `rem` m))) "","sine", showFFloat (Just 4) note02 "",
                   "vol", if compare (V.unsafeIndex v21 (j `rem` m)) 0.0 == GT then showFFloat (Just 4) (if dAmpl > 0.5 then 0.5 else dAmpl / 2) "" else "0"] ""
          if isNothing note1 then do { _ <- soxSynthHelpMain0 note0
                                     ; overSoXSynthHelpN v0 }
          else do { _ <- soxSynthHelpMain0 note0
                  ; _ <- soxSynthHelpMain1 (fromJust note1)
                  ; overSoXSynthHelpN v0
                  ; overSoXSynthHelpN2 v1}
          paths0 <- listDirectory "."
          let paths = sort . filter (isPrefixOf "test") $ paths0
          _ <- readProcessWithExitCode (fromJust (showE "sox")) (["--combine", "mix"] ++ paths ++ ["result0" ++ prependZeroes zeroN (show j) ++ ".wav","vol","0.3"]) ""
          mapM_ removeFile paths) vec0
 | otherwise = let ampL1 = ampL - (fromIntegral . truncate $ ampL) in
    if abs ampL1 < 0.01 then overSoXSynthN35G n 0.01 dAmpl v2 tts v3 vec0
    else overSoXSynthN35G n ampL1 dAmpl v2 tts v3 vec0

-- | Variant of the 'overSoXSynthN34G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition.
overSoXSynthN34GS :: Int -> Float -> Float -> Float -> String -> String -> String -> V.Vector Float -> IO ()
overSoXSynthN34GS n ampL time3 dAmpl zs = overSoXSynthN34G n ampL dAmpl (str2Durations zs time3)

-- | Variant of the 'overSoXSynthN34G' where intervals are obtained from the basic 'Intervals' with the length no more than 29 and a Ukrainian text 
-- specified as the last 'String' argument so that you can produce 'Intervals' of the arbitrary length. For more information, please, refer to 
-- 'intervalsFromStringG' and 'strToIntG'.
overSoXSynthN35GS :: Int -> Float -> Float -> Float -> String -> String -> Intervals -> String -> V.Vector Float -> IO ()
overSoXSynthN35GS n ampL time3 dAmpl zs tts v3 vs = overSoXSynthN35G n ampL dAmpl (str2Durations zs time3) tts (intervalsFromStringG v3 vs)

-- | Similar to 'overSoXSynthN', but uses a sound file to obtain the information analogous to 'V.Vector' in the latter one. Besides, the function lifts
-- the frequencies to the octave with the given by 'Int' parameter number (better to use from the range [1..8]). The first 'Float' argument from
-- the range [0.01..1.0] is used as a maximum amplitude for Overtones. If it is set to 1.0 the overTones amplitudes are just maximum ones,
-- otherwise they are multiplied by the parameter and this results in their becoming more silent ones.
-- The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" and \"end.wav\" files in the current directory, because they can be overwritten.
--
-- For better usage the 'FilePath' should be a filepath for the .wav file.
overSoXSynthNGen :: FilePath -> Int -> Float -> Float -> String -> IO ()
overSoXSynthNGen file m = overSoXSynthNGenE file m 12
  
-- | Similar to 'overSoXSynthNGen', but uses additional second 'Int' parameter. It defines, to which n-th elements set (see 'nkyT') belongs the obtained
-- higher notes in the intervals. If that parameter equals to 12, then the function is practically equivalent to 'overSoXSynthNGen'. To obtain
-- its modifications, please, use 2, 3, 4, 6, or 9.
overSoXSynthNGenE :: FilePath -> Int -> Int -> Float -> Float -> String -> IO ()
overSoXSynthNGenE file m ku ampL time3 zs = do
  n <- duration1000 file
  nGenE4Gi n file m ku ampL (str2DurationsDef n zs time3)

-- | Generalized version of the 'overSoXSynthNGenE' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGenEPar :: FilePath -> Params -> Float -> Float -> String -> IO ()
overSoXSynthNGenEPar file params ampL time3 zs = do
  n <- duration1000 file
  nGenE4GiPar n file params ampL (str2DurationsDef n zs time3)

-- | Variant of the 'overSoXSynthNGenE4G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition.
overSoXSynthNGenE4GS :: FilePath -> Int -> Int -> Float -> Float -> String -> IO ()
overSoXSynthNGenE4GS file m ku ampL time3 zs = do
  n <- duration1000 file
  nGenE4Gi n file m ku ampL (str2Durations zs time3)

-- | Generalized version of the 'overSoXSynthNGenE4GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGenE4GSPar :: FilePath -> Params -> Float -> Float -> String -> IO ()
overSoXSynthNGenE4GSPar file params ampL time3 zs = do
  n <- duration1000 file
  nGenE4GiPar n file params ampL (str2Durations zs time3)  

-- | Note that the last two 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The third 'Int' parameter defines that @n@.
nGenE4Gi :: Int -> FilePath -> Int -> Int -> Float -> Durations -> IO ()
nGenE4Gi n file m ku ampL v2 = do
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
  overSoXSynthN4G n ampL v2 vecB
  endFromResult  
    
-- | Generalized version of the 'nGenE4Gi' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
nGenE4GiPar :: Int -> FilePath -> Params -> Float -> Durations -> IO ()
nGenE4GiPar n file params ampL v2 = do
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
  overSoXSynthN4G n ampL v2 vecB
  endFromResult      
       
-- | 4G genaralized version of the 'overSoXSynthNGenE' where you provide your own 'Durations'.
overSoXSynthNGenE4G :: FilePath -> Int -> Int -> Float -> Durations -> IO ()
overSoXSynthNGenE4G file m ku ampL v2 = do
  n <- duration1000 file
  nGenE4Gi n file m ku ampL v2

-- | Generalized version of the 'overSoXSynthNGenE4G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGenE4GPar :: FilePath -> Params -> Float -> Durations -> IO ()
overSoXSynthNGenE4GPar file params ampL v2 = do
  n <- duration1000 file
  nGenE4GiPar n file params ampL v2  

-- | Similar to 'overSoXSynthN2', but uses a sound file to obtain the information analogous to 'V.Vector' in the latter one. Besides, the function lifts
-- the frequencies to the octave with the given by 'Int' parameter number (better to use from the range [1..8]). The first 'Float' argument from
-- the range [0.01..1.0] is used as a maximum amplitude for Overtones. If it is set to 1.0 the overTones amplitudes are just maximum ones,
-- otherwise they are multiplied by the parameter and this results in their becoming more silent ones.
-- The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" and \"end.wav\" files in the current directory, because they can be overwritten.
-- 
-- For better usage the 'FilePath' should be a filepath for the .wav file.
-- The second 'String' argument is used to define signs of the harmonics coefficients in the generated sounds.
overSoXSynthNGen2 :: FilePath -> Int -> Float -> Float -> String -> String -> IO ()
overSoXSynthNGen2 file m = overSoXSynthNGen2E file m 12
  
-- | Similar to 'overSoXSynthNGen2', but uses additional second 'Int' parameter. It defines, to which n-th elements set (see 'nkyT') belongs the obtained
-- higher notes in the intervals. If that parameter equals to 12, then the function is practically equivalent to 'overSoXSynthNGen2'. To obtain
-- its modifications, please, use 2, 3, 4, 6, or 9.
overSoXSynthNGen2E :: FilePath -> Int -> Int -> Float -> Float -> String -> String -> IO ()
overSoXSynthNGen2E file m ku ampL time3 zs tts = do
  n <- duration1000 file
  nGen2E4Gi n file m ku ampL (str2DurationsDef n zs time3) tts

-- | Generalized version of the 'overSoXSynthNGen2E' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen2EPar :: FilePath -> Params -> Float -> Float -> String -> String -> IO ()
overSoXSynthNGen2EPar file params ampL time3 zs tts = do
  n <- duration1000 file
  nGen2E4GiPar n file params ampL (str2DurationsDef n zs time3) tts  

-- | Variant of the 'overSoXSynthNGen2E4G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition.
overSoXSynthNGen2E4GS :: FilePath -> Int -> Int -> Float -> Float -> String -> String -> IO ()
overSoXSynthNGen2E4GS file m ku ampL time3 zs tts = do
  n <- duration1000 file
  nGen2E4Gi n file m ku ampL (str2Durations zs time3) tts

-- | Generalized version of the 'overSoXSynthNGen2E4GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen2E4GSPar :: FilePath -> Params -> Float -> Float -> String -> String -> IO ()
overSoXSynthNGen2E4GSPar file params ampL time3 zs tts = do
  n <- duration1000 file
  nGen2E4GiPar n file params ampL (str2Durations zs time3) tts

-- | Note that the last two 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The third 'Int' parameter defines that @n@.
nGen2E4Gi :: Int -> FilePath -> Int -> Int -> Float -> Durations -> String -> IO ()
nGen2E4Gi n file m ku ampL v2 tts = do
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
  overSoXSynthN24G n ampL v2 tts vecB
  endFromResult 

-- | Generalized version of the 'nGen2E4Gi' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
nGen2E4GiPar :: Int -> FilePath -> Params -> Float -> Durations -> String -> IO ()
nGen2E4GiPar n file params ampL v2 tts = do
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
  overSoXSynthN24G n ampL v2 tts vecB
  endFromResult   
       
-- | 4G genaralized version of the 'overSoXSynthNGen2E' where you provide your own 'Durations'.
overSoXSynthNGen2E4G :: FilePath -> Int -> Int -> Float -> Durations -> String -> IO ()
overSoXSynthNGen2E4G file m ku ampL v2 tts = do
  n <- duration1000 file
  nGen2E4Gi n file m ku ampL v2 tts

-- | Generalized version of the 'overSoXSynthNGen2E4G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen2E4GPar :: FilePath -> Params -> Float -> Durations -> String -> IO ()
overSoXSynthNGen2E4GPar file params ampL v2 tts = do
  n <- duration1000 file
  nGen2E4GiPar n file params ampL v2 tts  

-- | Similar to 'overSoXSynthN2', but uses a sound file to obtain the information analogous to 'V.Vector' in the latter one. Besides, the function lifts
-- the frequencies to the octave with the given by 'Int' parameter number (better to use from the range [1..8]). The first 'Float' argument from
-- the range [0.01..1.0] is used as a maximum amplitude for Overtones. If it is set to 1.0 the overTones amplitudes are just maximum ones,
-- otherwise they are multiplied by the parameter and this results in their becoming more silent ones.
-- The second 'Float' argument is a basic sound duration. The default one is 0.5 (second). Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" and \"end.wav\" files in the current directory, because they can be overwritten.
-- 
-- For better usage the 'FilePath' should be a filepath for the .wav file.
-- The second 'String' argument is used to define signs of the harmonics coefficients in the generated sounds.
-- The third 'String' argument is used to define the intervals for the notes if any.
-- The third 'Float' parameter basically is used to define in how many times the volume for the second lower note is less than the volume of
-- the main note. If it is rather great, it can signal that the volume for the second note overTones are greater than for the main note obetones.
-- The last one is experimental feature.
overSoXSynthNGen3 :: FilePath -> Int -> Float -> Float -> Float -> String -> String -> String -> IO ()
overSoXSynthNGen3 file m = overSoXSynthNGen3E file m 12
  
-- | Similar to 'overSoXSynthNGen3', but uses additional second 'Int' parameter. It defines, to which n-th elements set (see 'nkyT') belongs the obtained
-- higher notes in the intervals. If that parameter equals to 12, then the function is practically equivalent to 'overSoXSynthNGen3'. To obtain
-- its modifications, please, use 2, 3, 4, 6, or 9.
overSoXSynthNGen3E :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> String -> String -> IO ()
overSoXSynthNGen3E file m ku ampL time3 dAmpl zs tts vs = do
  n <- duration1000 file
  nGen3E4Gi n file m ku ampL dAmpl (str2DurationsDef n zs time3) tts vs

-- | Generalized version of the 'overSoXSynthNGen3E' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen3EPar :: FilePath -> Params -> Float -> Float -> Float -> String -> String -> String -> IO ()
overSoXSynthNGen3EPar file params ampL time3 dAmpl zs tts vs = do
  n <- duration1000 file
  nGen3E4GiPar n file params ampL dAmpl (str2DurationsDef n zs time3) tts vs  

-- | Variant of the 'overSoXSynthNGen3E4G' where 'Durations' are obtained from the 'String' using 'str2Durations' function. Helps to create a speech-like 
-- composition. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthNGen3E4GS :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> String -> String -> IO ()
overSoXSynthNGen3E4GS file m ku ampL time3 dAmpl zs tts vs = do
  n <- duration1000 file
  nGen3E4Gi n file m ku ampL dAmpl (str2Durations zs time3) tts vs 

-- | Generalized version of the 'overSoXSynthNGen3E4GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen3E4GSPar :: FilePath -> Params -> Float -> Float -> Float -> String -> String -> String -> IO ()
overSoXSynthNGen3E4GSPar file params ampL time3 dAmpl zs tts vs = do
  n <- duration1000 file
  nGen3E4GiPar n file params ampL dAmpl (str2Durations zs time3) tts vs    

-- | Note that the last two 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The third 'Int' parameter defines that @n@.
nGen3E4Gi :: Int -> FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> String -> IO ()
nGen3E4Gi n file m ku ampL dAmpl v2 tts vs = do 
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
  overSoXSynthN34G n ampL dAmpl v2 tts vs vecB
  endFromResult  

-- | Generalized version of the 'nGen3E4Gi' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
nGen3E4GiPar :: Int -> FilePath -> Params -> Float -> Float -> Durations -> String -> String -> IO ()
nGen3E4GiPar n file params ampL dAmpl v2 tts vs = do 
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
  overSoXSynthN34G n ampL dAmpl v2 tts vs vecB
  endFromResult    

-- | Note that the last two 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The third 'Int' parameter defines that @n@.
nGen3E5Gi :: Int -> FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> Intervals -> IO ()
nGen3E5Gi n file m ku ampL dAmpl v2 tts v3 = do 
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
  overSoXSynthN35G n ampL dAmpl v2 tts v3 vecB
  endFromResult  
    
-- | Generalized version of the 'nGen3E5Gi' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
nGen3E5GiPar :: Int -> FilePath -> Params -> Float -> Float -> Durations -> String -> Intervals -> IO ()
nGen3E5GiPar n file params ampL dAmpl v2 tts v3 = do 
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
  overSoXSynthN35G n ampL dAmpl v2 tts v3 vecB
  endFromResult      

-- | 4G generalized function for 'overSoXSynthNGen3E' where you provide your own 'Durations'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthNGen3E4G :: FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> String -> IO ()
overSoXSynthNGen3E4G file m ku ampL dAmpl v2 tts vs = do
  n <- duration1000 file
  nGen3E4Gi n file m ku ampL dAmpl v2 tts vs

-- | Generalized version of the 'overSoXSynthNGen3E4G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen3E4GPar :: FilePath -> Params -> Float -> Float -> Durations -> String -> String -> IO ()
overSoXSynthNGen3E4GPar file params ampL dAmpl v2 tts vs = do
  n <- duration1000 file
  nGen3E4GiPar n file params ampL dAmpl v2 tts vs  

-- | 5G generalized function for 'overSoXSynthNGen3E4G' where you provide your own 'Intervals'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthNGen3E5G :: FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> Intervals -> IO ()
overSoXSynthNGen3E5G file m ku ampL dAmpl v2 tts v3 = do
  n <- duration1000 file
  nGen3E5Gi n file m ku ampL dAmpl v2 tts v3 

-- | Generalized version of the 'overSoXSynthNGen3E5G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen3E5GPar :: FilePath -> Params -> Float -> Float -> Durations -> String -> Intervals -> IO ()
overSoXSynthNGen3E5GPar file params ampL dAmpl v2 tts v3 = do
  n <- duration1000 file
  nGen3E5GiPar n file params ampL dAmpl v2 tts v3    

-- | Variant of the 'overSoXSynthNGen3E5G' where 'Intervals' are obtained from the basic 'Intervals' with the length no more than 29 and a Ukrainian text 
-- specified as the last 'String' argument so that you can produce 'Intervals' of the arbitrary length. For more information, please, refer to 
-- 'intervalsFromStringG' and 'strToIntG'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthNGen3E5GS :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> String -> Intervals -> String -> IO ()
overSoXSynthNGen3E5GS file m ku ampL time3 dAmpl zs tts v3 vs = do
  n <- duration1000 file
  nGen3E5Gi n file m ku ampL dAmpl (str2Durations zs time3) tts (intervalsFromStringG v3 vs)

-- | Generalized version of the 'overSoXSynthNGen3E5GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen3E5GSPar :: FilePath -> Params -> Float -> Float -> Float -> String -> String -> Intervals -> String -> IO ()
overSoXSynthNGen3E5GSPar file params ampL time3 dAmpl zs tts v3 vs = do
  n <- duration1000 file
  nGen3E5GiPar n file params ampL dAmpl (str2Durations zs time3) tts (intervalsFromStringG v3 vs)  
  
-- | 6G generalized function for 'overSoXSynthNGen3E5G' where you provide your own 'Strengths'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthNGen3E6G :: FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> Intervals -> Strengths -> Float -> IO ()
overSoXSynthNGen3E6G file m ku ampL dAmpl v2 tts v3 v6 limV = 
 overSoXSynthNGen3E5G file m ku ampL dAmpl v2 tts v3 >> apply6G2 v6 "221w" "result" limV >> endFromResult

-- | Generalized version of the 'overSoXSynthNGen3E6G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen3E6GPar :: FilePath -> Params -> Float -> Float -> Durations -> String -> Intervals -> Strengths -> Float -> IO ()
overSoXSynthNGen3E6GPar file params ampL dAmpl v2 tts v3 v6 limV = 
 overSoXSynthNGen3E5GPar file params ampL dAmpl v2 tts v3 >> apply6G2 v6 "221w" "result" limV >> endFromResult 

-- | A variant of 'overSoXSynthNGen3E6G' where 'Strengths' are obtained from a Ukrainian text specified as the last 'String' argument. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthNGen3E6GS :: FilePath -> Int -> Int -> Float -> Float -> Durations -> String -> Intervals -> String -> Float -> IO ()
overSoXSynthNGen3E6GS file m ku ampL dAmpl v2 tts v3 xxs limV = overSoXSynthNGen3E6G file m ku ampL dAmpl v2 tts v3 (str2Volume xxs) limV

-- | Generalized version of the 'overSoXSynthNGen3E6GS' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen3E6GSPar :: FilePath -> Params -> Float -> Float -> Durations -> String -> Intervals -> String -> Float -> IO ()
overSoXSynthNGen3E6GSPar file params ampL dAmpl v2 tts v3 xxs limV = overSoXSynthNGen3E6GPar file params ampL dAmpl v2 tts v3 (str2Volume xxs) limV

-- | A variant of 'overSoXSynthNGen3E6GS' where 'Strengths' and 'Durations' are obtained from the same Ukrainian text specified as 
-- the last 'String' argument. The third 'Float' argument is an average duration of the sounds in seconds. 
-- Note that 'Int' arguments are used by 'liftInEnku' in that order so it returns a 'Maybe' number (actually frequency) for 
-- the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthNGen3E6GSu :: FilePath -> Int -> Int -> Float -> Float -> Float -> String -> Intervals -> String -> Float -> IO ()
overSoXSynthNGen3E6GSu file m ku ampL dAmpl time3 tts v3 xxs = overSoXSynthNGen3E6G file m ku ampL dAmpl (str2Durations xxs time3) tts v3 (str2Volume xxs)

-- | Generalized version of the 'overSoXSynthNGen3E6GSu' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthNGen3E6GSuPar :: FilePath -> Params -> Float -> Float -> Float -> String -> Intervals -> String -> Float -> IO ()
overSoXSynthNGen3E6GSuPar file params ampL dAmpl time3 tts v3 xxs = overSoXSynthNGen3E6GPar file params ampL dAmpl (str2Durations xxs time3) tts v3 (str2Volume xxs)
