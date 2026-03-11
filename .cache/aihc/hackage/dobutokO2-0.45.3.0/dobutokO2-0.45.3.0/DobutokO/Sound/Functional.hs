-- |
-- Module      :  DobutokO.Sound.Functional
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Functional (
  -- * Use additional function as a parameter
  overSoXSynth2FDN
  , overSoXSynth2FDN_B
  -- ** Just simple function application
  , overSoXSynth2FDN_S
  -- *** With additional filtering
  , overSoXSynth2FDN_Sf
  , overSoXSynth2FDN_Sf3
  -- * Use additional function and Ukrainian texts and generates melody
  , overSoXSynthGen2FDN
  , overSoXSynthGen2FDN_B
  , overSoXSynthGen2FDN_S
  , overSoXSynthGen2FDN_Sf
  , overSoXSynthGen2FDN_Sf3
  -- * 1G generalized functions with dB volume overtones adjustments
  , overSoXSynth2FDN1G
  , overSoXSynth2FDN_B1G
  , overSoXSynth2FDN_S1G
  , overSoXSynth2FDN_Sf1G
  , overSoXSynth2FDN_Sf31G
  -- * 2G generalized functions with additional sound quality specifying
  , overSoXSynth2FDN2G
  , overSoXSynth2FDN_B2G
  , overSoXSynth2FDN_S2G
  , overSoXSynth2FDN_Sf2G
  , overSoXSynth2FDN_Sf32G
  -- ** 2G generalized functions for melody producing
  , overSoXSynthGen2FDN_SG2G
  , overSoXSynthGen2FDN_Sf3G2G
  -- * Generalized functions with several functional parameters
  , soundGenF3
  , overSoXSynthGen2FDN_SG
  , overSoXSynthGen2FDN_Sf3G
  -- ** 1G generalized function with db volume overtones adjustments and several functional parameters
  , soundGenF31G
  -- * New functions for the version 0.36.0.0
  , overSoXSynthGen2FDN4G
  , overSoXSynthGen2FDN_SG4G
  , overSoXSynthGen2FDN_SG4GS
  -- * New generalized 5G functions that works with Intervals
  , overSoXSynth2FDN5G
  , overSoXSynth2FDN_B5G
  , overSoXSynth2FDN_S5G
  , overSoXSynth2FDN_Sf35G
  -- * New generalized 6G functions that works with Strengths
  , overSoXSynth2FDN6G
  , overSoXSynth2FDN6GS
  , overSoXSynth2FDN_B6G
  , overSoXSynth2FDN_B6GS
  , overSoXSynth2FDN_S6G
  , overSoXSynth2FDN_S6GS
  , overSoXSynth2FDN_Sf36G
  , overSoXSynth2FDN_Sf36GS
  , overSoXSynthGen2FDN_SG6G
  , overSoXSynthGen2FDN_SG6GS
  , overSoXSynthGen2FDN_SG6GSu
) where

import Numeric
import Data.List (nubBy)
import Data.Maybe (isNothing,fromJust,fromMaybe,maybe)
import qualified Data.Vector as V
import System.Process
import EndOfExe2
import System.Directory
import Aftovolio.Ukrainian.Melodics (convertToProperUkrainianI8)
import DobutokO.Sound.IntermediateF
import DobutokO.Sound.Functional.Basics
import DobutokO.Sound.Functional.Params
import DobutokO.Sound.DIS5G6G (intervalsFromStringG,str2Durations,str2Vol1,str2Volume)

-- | Similar to 'overSoXSynth2DN' but instead of 'overTones' function, it uses volatile function @f::Float -> Vector (Float, Float)@ with
-- somewhat sophisticated mechanism to normalize the resulting 'V.Vector' elements @(Float, Float)@. The last one is an experimental feature, so
-- it is your responsibility to provide a function so that it does not lead to clipping. In such a case, the result of application of the
-- 'convertToProperUkrainian' to the 'String' parameter must not be 'V.empty'. 'Int' argument is an index of the element to be taken from 
-- the 'intervalsFromString' applied to the 'String' argument. To obtain compatible with versions prior to 0.20.0.0 behaviour, use for the 'Int' 0.
--
-- Be aware that the result can be rather unpredictable (the program can even obtain segmentation fault) for not very suitable function.
-- But for a lot of functions this works well.
-- 
-- It is recommended to fully simplify the computation for \"f\" function before using it in the 'overSoXSynth2FDN'.
overSoXSynth2FDN :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()
overSoXSynth2FDN f (x, y) j zs = overSoXSynth2FDN1G f (x, y) j zs (V.replicate (V.length . f . closestNote $ if x /= 0.0 
  then abs x else V.unsafeIndex notes 0) 0.0)

-- | 'V.Vector' of 'Float' is a vector of dB volume adjustments for the corresponding harmonices (overtones).
overSoXSynth2FDN1G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> V.Vector Float -> IO ()
overSoXSynth2FDN1G f (x, y) j zs vdB = overSoXSynth2FDN2G f (x, y) j zs vdB []
 
-- | Similar to 'overSoXSynth2FDN1G', but additionally allows to specify by the second 'String' argument a quality changes to the generated files
-- (please, see 'soxBasicParams'). Since version 0.36.0.0 the function supports generation of the pauses.
overSoXSynth2FDN2G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN2G f (x, y) j zs vdB ys
 | null . convertToProperUkrainianI8 $ zs = overSoXSynth x
 | otherwise = do
    let note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        l0     = length zs
        note1 = dNote (V.unsafeIndex (intervalsFromString zs) (abs (j `rem` l0))) note0
        g0    = V.fromList . nubBy (\(!x1,_) (!x2,_) -> x1 == x2) . V.toList . V.map (\(noteX, !amplX) ->
           if noteX <= 0.0 then (2 * note0, abs (amplX - (fromIntegral . truncate $ amplX))) else (closestNote noteX,
             abs (amplX - (fromIntegral . truncate $ amplX)))) . f
        g k   = V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 k) $ 0), z0)) . g0 $ k 
        v0    = g note0
        v1    = maybe V.empty g note1
        ts = showFFloat (Just 4) (abs y) "" 
        overSoXSynthHelp = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "", "vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"]) "")
        overSoXSynthHelp2 vec vdB = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) (adjust_dbVol ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "","vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"] (V.unsafeIndex vdB i))) "") vec
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts, 
       "sine", showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then overSoXSynthHelp v0
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts, 
         "sine",  showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      overSoXSynthHelp v0
      overSoXSynthHelp2 v1 vdB
    if null ys then mixTest else mixTest2G ys   

-- | Similar to 'overSoXSynth2FDN2G', but additionally allows to specify by the 'Intervals' argument to specify your own intervals. For more information, 
-- please, refer to 'intervalsFromStringG'.
overSoXSynth2FDN5G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> Intervals -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN5G f (x, y) j v5 vdB ys
 | V.null v5 = overSoXSynth x
 | otherwise = do
    let note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        l0     = V.length v5
        note1 = dNote (V.unsafeIndex v5 (abs (j `rem` l0))) note0
        g0    = V.fromList . nubBy (\(!x1,_) (!x2,_) -> x1 == x2) . V.toList . V.map (\(noteX, !amplX) ->
           if noteX <= 0.0 then (2 * note0, abs (amplX - (fromIntegral . truncate $ amplX))) else (closestNote noteX,
             abs (amplX - (fromIntegral . truncate $ amplX)))) . f
        g k   = V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 k) $ 0), z0)) . g0 $ k 
        v0    = g note0
        v1    = maybe V.empty g note1
        ts = showFFloat (Just 4) (abs y) "" 
        overSoXSynthHelp = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "", "vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"]) "")
        overSoXSynthHelp2 vec vdB = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) (adjust_dbVol ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "","vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"] (V.unsafeIndex vdB i))) "") vec
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts, 
       "sine", showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then overSoXSynthHelp v0
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts, 
         "sine",  showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      overSoXSynthHelp v0
      overSoXSynthHelp2 v1 vdB
    if null ys then mixTest else mixTest2G ys       

-- | Generalized variant of the 'overSoXSynth2FDN5G' with afterwards 'apply6Gf' usage. 
overSoXSynth2FDN6G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> Intervals -> V.Vector Float -> String -> Float -> IO ()
overSoXSynth2FDN6G f (x, y) j v5 vdB ys vol
 | V.null v5 = overSoXSynth x
 | otherwise = do
    let note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        l0     = V.length v5
        note1 = dNote (V.unsafeIndex v5 (abs (j `rem` l0))) note0
        g0    = V.fromList . nubBy (\(!x1,_) (!x2,_) -> x1 == x2) . V.toList . V.map (\(noteX, !amplX) ->
           if noteX <= 0.0 then (2 * note0, abs (amplX - (fromIntegral . truncate $ amplX))) else (closestNote noteX,
             abs (amplX - (fromIntegral . truncate $ amplX)))) . f
        g k   = V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 k) $ 0), z0)) . g0 $ k 
        v0    = g note0
        v1    = maybe V.empty g note1
        ts = showFFloat (Just 4) (abs y) "" 
        overSoXSynthHelp = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "", "vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"]) "")
        overSoXSynthHelp2 vec vdB = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) (adjust_dbVol ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "","vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"] (V.unsafeIndex vdB i))) "") vec
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts, 
       "sine", showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then overSoXSynthHelp v0
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts, 
         "sine",  showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      overSoXSynthHelp v0
      overSoXSynthHelp2 v1 vdB
    if null ys then mixTest else mixTest2G ys
    if compare y 0.0 == GT then apply6Gf vol ("result." ++ if drop 3 ys == "f" then "flac" else "wav") else putStr ""

-- | A variant of the 'overSoXSynth2FDN6G' where volume adjustment is obtained from a Ukrainian text.
overSoXSynth2FDN6GS :: (Float -> OvertonesO) -> (Float, Float) -> Int -> Intervals -> String -> V.Vector Float -> String -> String -> IO ()
overSoXSynth2FDN6GS f (x, y) j v5 xs vdB ys xxs 
 | null . convertToProperUkrainianI8 $ xxs = putStrLn "You provided no information to obtain volume adjustment! "
 | otherwise = overSoXSynth2FDN6G f (x, y) j (intervalsFromStringG v5 xs) vdB ys (str2Vol1 xxs)
 
-- | Uses additional 'Int' parameters. The first one is a number of enka (see 'nkyT'). The second one defines, to which n-th elements set
-- (see 'nkyT') belongs the obtained higher notes in the intervals. To obtain reasonable results, please, use for the first one 2, 3, 4, 6, 9, or 12.
-- The first 'String' parameter is used to produce durations of the notes. The second one is used to define intervals. A 'Float' parameter is a
-- basic sound duration, it defines tempo of the melody in general.
overSoXSynthGen2FDN :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> String -> String -> IO ()
overSoXSynthGen2FDN file m ku f y zs wws = overSoXSynthGen2FDN_SG file m ku f y zs wws overSoXSynth2FDN

-- | Generalized variant of the 'overSoXSynthGen2FDN' with your own specified 'Durations' for the sounds and pauses. 
-- Instead of using a Ukrainian text to specify a durations for the sounds (and a rhythm 
-- respectively) you provide your own rhythm as 'Durations'. Positive values correspond to durations of the sounds generated 
-- and negative values -- to durations of the pauses respectively. 
overSoXSynthGen2FDN4G :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Durations -> String -> IO ()
overSoXSynthGen2FDN4G file m ku f v2 wws = overSoXSynthGen2FDN_SG4G file m ku f v2 wws overSoXSynth2FDN

-- | Similar to 'overSoXSynth2DN' but instead of 'overTones' function, it uses volatile function @f::Float -> Vector (Float, Float)@ with
-- somewhat sophisticated mechanism to normalize the resulting 'V.Vector' elements @(Float, Float)@. The last one is experimental feature, so
-- it is your responsibility to provide a function so that it does not lead to clipping. In such a case, the result of application of the
-- 'convertToProperUkrainian' to the 'String' parameter must not be 'V.empty'. The function also tries to perform filtering to avoid possible beating.
-- The third 'Float' parameter in the tuple is used as a limit for frequencies difference in Hz to be filtered out from the resulting sound. It is
-- considered to be from the range @[0.1..10.0]@. An 'Int' parameter is used to define the needed interval. To obtain compatible with versions prior
-- to 0.20.0.0 behaviour, use for the 'Int' 0.
--
-- Be aware that the result can be rather unpredictable (the program can even obtain segmentation fault) for not very suitable function.
-- But for a lot of functions this works well.
-- 
-- It is recommended to fully simplify the computation for \"f\" function before using it in the 'overSoXSynth2FDN_B'.
overSoXSynth2FDN_B :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> IO ()
overSoXSynth2FDN_B f (x, y, limB) j zs = overSoXSynth2FDN_B1G f (x, y, limB) j zs (V.replicate (V.length . f . closestNote $ if x /= 0.0 then abs x else V.unsafeIndex notes 0) 0.0)

-- | 'V.Vector' of 'Float' is a vector of dB volume adjustments for the corresponding harmonices (overtones).
overSoXSynth2FDN_B1G :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> V.Vector Float -> IO ()
overSoXSynth2FDN_B1G f (x, y, limB) j zs vdB = overSoXSynth2FDN_B2G f (x, y, limB) j zs vdB []

-- | Generalized version of the 'overSoXSynth2FDN_B1G' with a possibility to specify sound quality parameters using additional second 'String'
-- argument. For more information, please, refer to 'soxBasicParams'. 
overSoXSynth2FDN_B2G :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN_B2G f (x, y, limB) j zs vdB ys
 | null . convertToProperUkrainianI8 $ zs = overSoXSynth x
 | otherwise = do
    let limA0 = abs ((limB / 10) - (fromIntegral . truncate $ (limB / 10))) * 10
        limA  = if compare limA0 0.1 == LT then 0.1 else limA0
        l0    = length zs
        note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        note1 = dNote (V.unsafeIndex (intervalsFromString zs) (abs (j `rem` l0))) note0
        g0    = V.fromList . nubBy (\(!x1,_) (!x2,_) -> compare (abs (x1 - x2)) limA == LT) . V.toList . V.map (\(noteX, !amplX) ->
           if noteX <= 0.0 then (2 * note0, abs (amplX - (fromIntegral . truncate $ amplX))) else (closestNote noteX,
             abs (amplX - (fromIntegral . truncate $ amplX)))) . f
        v0    = V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 note0) $ 0), z0)) . g0 $ note0 
        v1    = if isNothing note1 then V.empty
                else V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 . fromJust $ note1) $ 0), z0)) . g0 . fromJust $ note1
        ts = showFFloat (Just 4) (abs y) "" 
        overSoXSynthHelp = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "", "vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"]) "")
        overSoXSynthHelp2 vec vdB = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ((if V.null vdB then id else (\wwws -> adjust_dbVol wwws (V.unsafeIndex vdB i))) 
              ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", showFFloat Nothing noteN "", "vol", 
                if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"])) "") vec
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",
       showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then overSoXSynthHelp v0
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts,"sine",
         showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      overSoXSynthHelp v0
      overSoXSynthHelp2 v1 vdB
    if null ys then mixTest else mixTest2G ys    

-- | Generalized version of the 'overSoXSynth2FDN_B2G' with a possibility to specify your own 'Intervals'. For more information, please, 
-- refer to 'intervalsFromStringG'.
overSoXSynth2FDN_B5G :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> Intervals -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN_B5G f (x, y, limB) j v5 vdB ys
 | V.null v5 = overSoXSynth x
 | otherwise = do
    let limA0 = abs ((limB / 10) - (fromIntegral . truncate $ (limB / 10))) * 10
        limA  = if compare limA0 0.1 == LT then 0.1 else limA0
        l0    = V.length v5
        note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        note1 = dNote (V.unsafeIndex v5 (abs (j `rem` l0))) note0
        g0    = V.fromList . nubBy (\(!x1,_) (!x2,_) -> compare (abs (x1 - x2)) limA == LT) . V.toList . V.map (\(noteX, !amplX) ->
           if noteX <= 0.0 then (2 * note0, abs (amplX - (fromIntegral . truncate $ amplX))) else (closestNote noteX,
             abs (amplX - (fromIntegral . truncate $ amplX)))) . f
        v0    = V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 note0) $ 0), z0)) . g0 $ note0 
        v1    = if isNothing note1 then V.empty
                else V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 . fromJust $ note1) $ 0), z0)) . g0 . fromJust $ note1
        ts = showFFloat (Just 4) (abs y) "" 
        overSoXSynthHelp = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "", "vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"]) "")
        overSoXSynthHelp2 vec vdB = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ((if V.null vdB then id else (\wwws -> adjust_dbVol wwws (V.unsafeIndex vdB i))) 
              ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", showFFloat Nothing noteN "", "vol", 
                if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"])) "") vec
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",
       showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then overSoXSynthHelp v0
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts,"sine",
         showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      overSoXSynthHelp v0
      overSoXSynthHelp2 v1 vdB
    if null ys then mixTest else mixTest2G ys    

-- | Generalized variant of the 'overSoXSynth2FDN_B5G' with afterwards 'apply6G' usage. 
overSoXSynth2FDN_B6G :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> Intervals -> V.Vector Float -> String -> Float -> IO ()
overSoXSynth2FDN_B6G f (x, y, limB) j v5 vdB ys vol 
  | V.null v5 = overSoXSynth x
  | otherwise = do
    let limA0 = abs ((limB / 10) - (fromIntegral . truncate $ (limB / 10))) * 10
        limA  = if compare limA0 0.1 == LT then 0.1 else limA0
        l0    = V.length v5
        note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        note1 = dNote (V.unsafeIndex v5 (abs (j `rem` l0))) note0
        g0    = V.fromList . nubBy (\(!x1,_) (!x2,_) -> compare (abs (x1 - x2)) limA == LT) . V.toList . V.map (\(noteX, !amplX) ->
           if noteX <= 0.0 then (2 * note0, abs (amplX - (fromIntegral . truncate $ amplX))) else (closestNote noteX,
             abs (amplX - (fromIntegral . truncate $ amplX)))) . f
        v0    = V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 note0) $ 0), z0)) . g0 $ note0 
        v1    = if isNothing note1 then V.empty
                else V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) .
                   V.imap (\i (_,!z0) -> (fromIntegral (i + 1) * (fst . V.unsafeIndex (g0 . fromJust $ note1) $ 0), z0)) . g0 . fromJust $ note1
        ts = showFFloat (Just 4) (abs y) "" 
        overSoXSynthHelp = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", 
              showFFloat Nothing noteN "", "vol", if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"]) "")
        overSoXSynthHelp2 vec vdB = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
            ((if null ys then id else soxBasicParams ys) ((if V.null vdB then id else (\wwws -> adjust_dbVol wwws (V.unsafeIndex vdB i))) 
              ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", ts,"sine", showFFloat Nothing noteN "", "vol", 
                if compare y 0.0 == GT then showFFloat Nothing amplN "" else "0"])) "") vec
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",
       showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then overSoXSynthHelp v0
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts,"sine",
         showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      overSoXSynthHelp v0
      overSoXSynthHelp2 v1 vdB
    if null ys then mixTest else mixTest2G ys    
    if compare y 0.0 == GT then apply6Gf vol ("result." ++ if drop 3 ys == "f" then "flac" else "wav") else putStr ""

-- | A variant of the 'overSoXSynth2FDN_B6G' where volume adjustment is obtained from a Ukrainian text.
overSoXSynth2FDN_B6GS :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> Intervals -> String -> V.Vector Float -> String -> String -> IO ()
overSoXSynth2FDN_B6GS f (x, y, limB) j v5 xs vdB ys xxs
 | null . convertToProperUkrainianI8 $ xxs = putStrLn "You provided no information to obtain volume adjustment! "
 | otherwise = overSoXSynth2FDN_B6G f (x, y, limB) j (intervalsFromStringG v5 xs) vdB ys (str2Vol1 xxs)

-- | Uses additional 'Int' parameters. The first one is a number of enka (see 'nkyT'). The second one defines, to which n-th elements set
-- (see 'nkyT') belongs the obtained higher notes in the intervals. To obtain reasonable results, please, use for the first one 2, 3, 4, 6, 9, or 12.
-- The first 'String' parameter is used to produce durations of the notes. The second one is used to define intervals. The first 'Float' parameter is a
-- basic sound duration, it defines tempo of the melody in general. The second one is a limit for frequencies difference in Hz to be filtered out from the
-- resulting sound. It is considered to be from the range @[0.1..10.0]@.
overSoXSynthGen2FDN_B :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> Float -> String -> String -> IO ()
overSoXSynthGen2FDN_B file m ku f y limB zs wws = overSoXSynthGen2FDN_Sf3G file m ku f y limB zs wws overSoXSynth2FDN_B

-- | Similar to 'overSoXSynth2FDN' but it does not make any normalizing transformations with the 'V.Vector' argument. To be used properly, it is needed
-- that every second element in the tuple in the 'V.Vector' argument must be in the range [-1.0..1.0] and every first element must be in between
-- 16.351597831287414 and 7902.132820097988 (Hz). An 'Int' parameter is used to define an interval. To obtain compatible with versions prior to
-- 0.20.0.0 behaviour, use for the 'Int' 0.
--
-- Be aware that the result can be rather unpredictable (the program can even obtain segmentation fault) for not very suitable function.
-- But for a lot of functions this works well.
-- 
-- It is recommended to fully simplify the computation for \"f\" function before using it in the 'overSoXSynth2FDN_S'.
overSoXSynth2FDN_S :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()
overSoXSynth2FDN_S f (x, y) j zs = overSoXSynth2FDN_S2G f (x, y) j zs V.empty []

-- | Generalized version of the 'overSoXSynth2FDN_S' with the additional volume adjustment in dB for overtones given by 'V.Vector' of 'Float'.
overSoXSynth2FDN_S1G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> V.Vector Float -> IO ()
overSoXSynth2FDN_S1G f (x, y) j zs vdB = overSoXSynth2FDN_S2G f (x, y) j zs vdB []
 
-- | Generalized version of the 'overSoXSynth2FDN_S1G' with a possibility to specify sound quality parameters using the second 'String' argument.
-- For more information, please, refer to 'soxBasicParams'.
overSoXSynth2FDN_S2G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN_S2G f (x, y) j zs vdB ys
 | null . convertToProperUkrainianI8 $ zs = overSoXSynth x
 | otherwise = do
    let note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        l0    = length zs
        note1 = dNote (V.unsafeIndex (intervalsFromString zs) (abs (j `rem` l0))) note0
        v0    = f note0
        v1    = maybe V.empty f note1
        ts = showFFloat (Just 4) (abs y) ""
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",
       showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then partialTest_k2G v0 0 ts vdB ys
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts,"sine",
         showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      partialTest_k2G v0 0 ts vdB ys
      partialTest_k2G v1 1 ts vdB ys
    if null ys then mixTest else mixTest2G ys    
        
-- | Generalized version of the 'overSoXSynth2FDN_S2G' where you specify your own 'Intervals'. For more information, please, refer 
-- to 'intervalsFromStringG'.
overSoXSynth2FDN_S5G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> Intervals -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN_S5G f (x, y) j v5 vdB ys
 | V.null v5 = overSoXSynth x
 | otherwise = do
    let note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        l0    = V.length v5
        note1 = dNote (V.unsafeIndex v5 (abs (j `rem` l0))) note0
        v0    = f note0
        v1    = maybe V.empty f note1
        ts = showFFloat (Just 4) (abs y) ""
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",
       showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then partialTest_k2G v0 0 ts vdB ys
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts,"sine",
         showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      partialTest_k2G v0 0 ts vdB ys
      partialTest_k2G v1 1 ts vdB ys
    if null ys then mixTest else mixTest2G ys            

-- | Generalized variant of the 'overSoXSynth2FDN_S5G' with afterwards 'apply6G' usage. Arguments for the latter is the three last function arguments.
overSoXSynth2FDN_S6G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> Intervals -> V.Vector Float -> String -> Float -> IO ()
overSoXSynth2FDN_S6G f (x, y) j v5 vdB ys vol
 | V.null v5 = overSoXSynth x
 | otherwise = do
    let note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        l0    = V.length v5
        note1 = dNote (V.unsafeIndex v5 (abs (j `rem` l0))) note0
        v0    = f note0
        v1    = maybe V.empty f note1
        ts = showFFloat (Just 4) (abs y) ""
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",
       showFFloat Nothing note0 "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
    if isNothing note1 then partialTest_k2G v0 0 ts vdB ys
    else do 
      _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ["-r22050", "-n", "testB.wav", "synth", ts,"sine",
         showFFloat Nothing (fromJust note1) "","vol", if compare y 0.0 == GT then "1.0" else "0"]) ""
      partialTest_k2G v0 0 ts vdB ys
      partialTest_k2G v1 1 ts vdB ys
    if null ys then mixTest else mixTest2G ys            
    if compare y 0.0 == GT then apply6Gf vol ("result." ++ if drop 3 ys == "f" then "flac" else "wav") else putStr ""
  
-- | A variant of the 'overSoXSynth2FDN_S6G' where volume adjustment is obtained from a Ukrainian text.
overSoXSynth2FDN_S6GS :: (Float -> OvertonesO) -> (Float, Float) -> Int -> Intervals -> String -> V.Vector Float -> String -> String -> IO ()
overSoXSynth2FDN_S6GS f (x, y) j v5 xs vdB ys xxs
 | null . convertToProperUkrainianI8 $ xxs = putStrLn "You provided no information to obtain volume adjustment! "
 | otherwise = overSoXSynth2FDN_S6G f (x, y) j (intervalsFromStringG v5 xs) vdB ys (str2Vol1 xxs)

-- | Similar to 'overSoXSynthGen2FDN', but instead of 'overSoXSynth2FDN' uses 'overSoXSynth2FDN_S' function. Note that the first 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthGen2FDN_SG :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> String -> String -> 
  ((Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()) -> IO ()
overSoXSynthGen2FDN_SG file m ku f y zs wws h = do
  n <- duration1000 file
  overSoXSynthGen2FDN_SG4G file m ku f (str2DurationsDef n zs y) wws h

-- | Generalized version of the 'overSoXSynthGen2FDN_SG' where instead of using a Ukrainian text to specify a durations for the sounds (and a rhythm 
-- respectively) you provide your own rhythm as 'Durations'. Positive values correspond to durations of the sounds generated 
-- and negative values -- to durations of the pauses respectively. Please, use a function @h :: ((Float -> OvertonesO) -> (Float, Float) -> 
-- Int -> String -> IO ())@ such that it can create for the given values accorgingly sounds and pauses. Otherwise, please, check whether at 
-- least it can deal with such arguments without errors. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthGen2FDN_SG4G :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Durations -> String -> 
  ((Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()) -> IO ()
overSoXSynthGen2FDN_SG4G file m ku f v2 wws h = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
      zeroN = numVZeroesPre vecB in V.imapM_ (\j x -> do
        h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) j wws
        renameFile "result.wav" $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult  

-- | A variant of the 'overSoXSynthGen2FDN_SG4G' where instead of providing your own durations as 'Durations' you use a Ukrainian text and 
-- a function treats each symbol in it as a duration parameter with its sign. Positive values correspond to durations of the sounds generated 
-- and negative values -- to durations of the pauses respectively. Please, use a function @h :: ((Float -> OvertonesO) -> (Float, Float) -> 
-- Int -> String -> IO ())@ such that it can create for the given values accorgingly sounds and pauses. Otherwise, please, check whether at 
-- least it can deal with such arguments without errors.
overSoXSynthGen2FDN_SG4GS :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> String -> String -> 
  ((Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()) -> IO ()
overSoXSynthGen2FDN_SG4GS file m ku f y zs = overSoXSynthGen2FDN_SG4G file m ku f (str2Durations zs y)

-- | 6G generalized variant of the 'overSoXSynthGen2FDN_SG4G' with volume adjustments given by 'Strengths'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthGen2FDN_SG6G :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Durations -> String -> 
  ((Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()) -> Strengths -> Float -> IO ()
overSoXSynthGen2FDN_SG6G file m ku f v2 wws h v6 limV
 | V.null v6 = putStrLn "You did not provide a volume adjustments vector! "
 | otherwise = do
    n <- duration1000 file
    vecA <- freqsFromFile file n
    let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
        zeroN = numVZeroesPre vecB in V.imapM_ (\j x -> do
          h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) j wws
          renameFile "result.wav" $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav"
          apply6GSilentFile ("result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") limV (V.unsafeIndex v6 (j `rem` V.length v6))) vecB
    endFromResult  

-- | A variant of the 'overSoXSynthGen2FDN_SG6G' where 'Strengths' are obtained from a Ukrainian text and 'str2Volume'.
overSoXSynthGen2FDN_SG6GS :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> String -> String -> 
  ((Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()) -> String -> Float -> IO ()
overSoXSynthGen2FDN_SG6GS file m ku f y zs wws h zzs = overSoXSynthGen2FDN_SG6G file m ku f (str2Durations zs y) wws h (str2Volume zzs)  

-- | A variant of the 'overSoXSynthGen2FDN_SG6GS' where 'Strengths' are obtained from the same Ukrainian text as also 'Durations' so the last 
-- 'String' argument is omitted (it is equal to the first one). Helps to create a speech-like composition.
overSoXSynthGen2FDN_SG6GSu :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> String -> String -> 
  ((Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()) -> Float -> IO ()
overSoXSynthGen2FDN_SG6GSu file m ku f y zs wws h = overSoXSynthGen2FDN_SG6G file m ku f (str2Durations zs y) wws h (str2Volume zs)

-- | Generalized variant of the 'overSoXSynthGen2FDN_SG' with a possibility to specify with the third 'String' argument sound quality parameters.
-- Besides, the second from the end argument (a function) needs to be one more argument -- just also 'String'. 
-- For more information, please, refer to 'soxBasicParams'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthGen2FDN_SG2G :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> String -> String -> ((Float -> OvertonesO) ->
  (Float, Float) -> Int -> String -> String -> IO ()) -> String -> IO ()
overSoXSynthGen2FDN_SG2G file m ku f y zs wws h ys = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j x -> do
        h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) j wws ys
        renameFile ("result." ++ if drop 3 ys == "f" then "flac" else "wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++
          if drop 3 ys == "f" then ".flac" else ".wav") vecB
  endFromResult2G ys  

-- | Similar to 'overSoXSynthGen2FDN', but instead of 'overSoXSynth2FDN' uses 'overSoXSynth2FDN_S' function. 
overSoXSynthGen2FDN_S :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> String -> String -> IO ()
overSoXSynthGen2FDN_S file m ku f y zs wws = overSoXSynthGen2FDN_SG file m ku f y zs wws overSoXSynth2FDN_S

-- | Generalized variant of the 'overSoXSynth2FDN_Sf' with a possibility to adjust volume using 'adjust_dbVol'. 'V.Vector' of 'Float' is
-- used to specify adjustments in dB. For more information, please, refer to 'adjust_dbVol'.
overSoXSynth2FDN_Sf1G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> V.Vector Float -> IO ()
overSoXSynth2FDN_Sf1G f (x, y) = overSoXSynth2FDN_Sf31G f (x, y, 0.001)

-- | Generalized variant of the 'overSoXSynth2FDN_Sf1G' with a possibility to specify sound quality using the second 'String' argument.
-- For more information, please, refer to 'soxBasicParams'.
overSoXSynth2FDN_Sf2G :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN_Sf2G f (x, y) = overSoXSynth2FDN_Sf32G f (x, y, 0.001)

-- | Similar to 'overSoXSynthGen2FDN_S', but instead of 'overSoXSynth2FDN_S' uses 'overSoXSynth2FDN_Sf' function. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthGen2FDN_Sf :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> String -> String -> IO ()
overSoXSynthGen2FDN_Sf file m ku f y zs wws = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j x -> do
        overSoXSynth2FDN_Sf f (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) j wws
        renameFile "result.wav" $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult  
   
-- | Generalized variant of the 'overSoXSynth2FDN_Sf3' function with a possibility to adjust volume using 'adjust_dBVol'. 'V.Vector' of 'Float'
-- specifies the needed adjustments in dB.
overSoXSynth2FDN_Sf31G :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> V.Vector Float -> IO ()
overSoXSynth2FDN_Sf31G f (x, y, t0) j zs vdB = overSoXSynth2FDN_Sf32G f (x, y, t0) j zs vdB []

-- | Generalized variant of the 'overSoXSynth2FDN_Sf31G' with a possibility to specify sound quality using the second 'String' parameter.
-- For more information, please, refer to 'soxBasicParams'.
overSoXSynth2FDN_Sf35G :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> Intervals -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN_Sf35G f (x, y, t0) j v5 vdB ys
 | V.null v5 = overSoXSynth x
 | otherwise = do
    let l0    = V.length v5
    soundGenF32G (V.fromList [\x -> closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0),\x -> fromMaybe (V.unsafeIndex notes 0)
     (dNote (V.unsafeIndex v5 (abs (j `rem` l0))) (closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)))])
       (V.replicate 2 x) (V.fromList [1,V.unsafeIndex v5 (abs (j `rem` l0))]) f (x, y, t0) j vdB ys
    if null ys then mixTest else mixTest2G ys 

-- | Generalized variant of the 'overSoXSynth2FDN_Sf35G' with afterwards 'apply6G' usage.
overSoXSynth2FDN_Sf36G :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> Intervals -> V.Vector Float -> String -> Float -> IO ()
overSoXSynth2FDN_Sf36G f (x, y, t0) j v5 vdB ys vol
 | V.null v5 = overSoXSynth x
 | otherwise = do
    let l0    = V.length v5
    soundGenF32G (V.fromList [\x -> closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0),\x -> fromMaybe (V.unsafeIndex notes 0)
     (dNote (V.unsafeIndex v5 (abs (j `rem` l0))) (closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)))])
       (V.replicate 2 x) (V.fromList [1,V.unsafeIndex v5 (abs (j `rem` l0))]) f (x, y, t0) j vdB ys
    if null ys then mixTest else mixTest2G ys 
    if compare y 0.0 == GT then apply6Gf vol ("result." ++ if drop 3 ys == "f" then "flac" else "wav") else putStr ""

-- | A variant of the 'overSoXSynth2FDN_Sf36G' where volume adjustment is obtained from a Ukrainian text.
overSoXSynth2FDN_Sf36GS :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> Intervals -> String -> V.Vector Float -> String -> String -> IO ()
overSoXSynth2FDN_Sf36GS f (x, y, t0) j v5 xs vdB ys xxs
 | null . convertToProperUkrainian $ xxs = putStrLn "You provided no information to obtain volume adjustment! "
 | otherwise = overSoXSynth2FDN_Sf36G f (x, y, t0) j (intervalsFromStringG v5 xs) vdB ys (str2Vol1 xxs)

-- | Can generate multiple notes with their respective overtones that are played simultaneously (e. g. it can be just one note with overtones,
-- an interval with overtones, an accord with overtones etc.). This allows to get a rather complex or even complicated behaviour to obtain expressive
-- and rich sound.
soundGenF3 :: V.Vector (Float -> Float) -> V.Vector Float -> V.Vector Int -> (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> IO ()
soundGenF3 vf vd vi f (x, y, t0) j = soundGenF32G vf vd vi f (x, y, t0) j V.empty []

-- | Generalized variant of the 'soundGenF3' with volume adjustment in dB given by the second @Vector Float@ for the overtones.
soundGenF31G :: V.Vector (Float -> Float) -> V.Vector Float -> V.Vector Int -> (Float -> OvertonesO) -> (Float, Float, Float) -> Int ->
  V.Vector Float -> IO ()
soundGenF31G vf vd vi f (x, y, t0) j vdB = soundGenF32G vf vd vi f (x, y, t0) j vdB [] 

-- | Similar to 'overSoXSynthGen2FDN_S', but instead of 'overSoXSynth2FDN_S' uses 'overSoXSynth2FDN_Sf3' function. 
overSoXSynthGen2FDN_Sf3 :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> Float -> String -> String -> IO ()
overSoXSynthGen2FDN_Sf3 file m ku f y t0 zs wws = overSoXSynthGen2FDN_Sf3G file m ku f y t0 zs wws overSoXSynth2FDN_Sf3

-- | Similar to 'overSoXSynthGen2FDN_S', but instead of 'overSoXSynth2FDN_S' uses 'overSoXSynth2FDN_Sf3' function. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthGen2FDN_Sf3G :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> Float -> String -> String ->
 ((Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> IO ()) -> IO ()
overSoXSynthGen2FDN_Sf3G file m ku f y t0 zs wws h = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j x -> do
        h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2))), t0) j wws
        renameFile "result.wav" $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult

-- | Generalized variant of the 'ovorSoXSynthGen2FDN_Sf3G' with a possibility to specify sound quality with the third 'String' argument.
-- Besides, the second from the end argument (a function) needs to be one more argument -- just also 'String'. 
-- For more information, please, refer to 'soxBasicParams'. Note that 'Int' arguments are used by 'liftInEnku' in that order so it 
-- returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT'). The second 'Int' parameter defines that @n@.
overSoXSynthGen2FDN_Sf3G2G :: FilePath -> Int -> Int -> (Float -> OvertonesO) -> Float -> Float -> String -> String ->
 ((Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> String -> IO ()) -> String -> IO ()
overSoXSynthGen2FDN_Sf3G2G file m ku f y t0 zs wws h ys = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInEnkuV m ku . V.map fromIntegral . V.filter (/= (11440::Int)) $ vecA
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j x -> do
        h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2))), t0) j wws ys
        renameFile ("result." ++ if drop 3 ys == "f" then "flac" else "wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ if drop 3 ys == "f"
          then ".flac" else ".wav") vecB
  endFromResult2G ys  

