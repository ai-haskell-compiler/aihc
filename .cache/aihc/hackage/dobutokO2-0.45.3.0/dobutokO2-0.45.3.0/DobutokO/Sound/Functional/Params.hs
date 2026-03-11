-- |
-- Module      :  DobutokO.Sound.Functional.Params
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside. Is more complicated than
-- dobutokO2 and uses its functionality.

{-# LANGUAGE BangPatterns, MultiWayIf #-}
{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Functional.Params (
  Params (..)
  -- * Type synonyms with different semantics
  , Durations
  , Strengths
  , Intervals
  -- * New generalizations for scales and modes with Params
  , filterInParams
  , sortNoDup
  , toneD
  , toneE
  , liftInParams
  , liftInParamsV
  , lengthP
  , elemP
  , elemCloseP
  , showD
  , isStrParams
  , isListParams
  -- ** Application of the Params
  , overSoXSynthGen2FDN_SG4GPar
  , overSoXSynthGen2FDN_SG6GPar
  , overSoXSynthGen2FDN_SG2GPar
  , overSoXSynthGen2FDN_SfPar
  , overSoXSynthGen2FDN_Sf3GPar
  , overSoXSynthGen2FDN_Sf3G2GPar  
  -- * Creating melody from overtones
  , overMeloPar
  -- * Additional functions
  , str2DurationsDef
  , signsFromString
  , apply6Gf
  , apply6GSilentFile
  , vStrToVIntG
  , strToIntG
  , defInt
  , syllableStr
  , overSoXSynth2FDN_Sf
  , overSoXSynth2FDN_Sf3
  , overSoXSynth2FDN_Sf32G
  , intervalsFromString
  , soundGenF32G
  , helpF0
  , helpF1
  , convertToProperUkrainian
  , newRepresentation
  , doubleVecFromVecOfFloat
) where

import CaseBi.Arr 
import GHC.Arr
import Numeric
import Data.List (sort)
import Data.Maybe (isNothing,fromJust,isJust,fromMaybe)
import qualified Data.Vector as V
import System.Process
import EndOfExe2
import System.Directory
import Aftovolio.Ukrainian.Melodics (convertToProperUkrainianI8)
import Sound.SoXBasics (upperBnd,selMaxAbs)
import MMSyn7l
import GHC.Int (Int8)
import Aftovolio.Ukrainian.Syllable 
import DobutokO.Sound.IntermediateF
import DobutokO.Sound.Functional.Basics

convertToProperUkrainian :: String -> V.Vector String
convertToProperUkrainian = newRepresentation . convertToProperUkrainianI8
{-# INLINE convertToProperUkrainian #-}

newRepresentation :: [Int8] -> V.Vector String
newRepresentation = V.fromList . map f
  where f = getBFstLSorted' "" [(1,"\1072"),(2,"\1077"),(3,"\1086"),(4,"\1091"),(5,"\1080"),(6,"\1110"),(7,"\1100"),(8,"\1076\1079"),(10,"\1078"),(15,"\1073"),(17,"\1076"),(19,"\1075"),(21,"\1169"),(23,"\1076\1078"),(25,"\1079"),(27,"\1081"),(28,"\1083"),(30,"\1084"),(32,"\1085"),(34,"\1088"),(36,"\1074"),(38,"\1094"),(39,"\1095"),(41,"\1096"),(43,"\1092"),(45,"\1082"),(47,"\1087"),(49,"\1089"),(50,"\1090"),(52,"\1093"),(54,"\1089\1100"),(66,"\1094\1100"),(100,"0"),(101,"1")]

-- | Representation of the scales and modes for the notes. Can be extended further, but for a lot of situations the following realization is sufficient. 
-- See, for example, 'filterInParams' and so on. 'String' is (are) used as a general classification name, for some of them there are provided two 
-- 'String' to classify. Lists are used to specify remainders in some meaning. See also, 'liftInParams' and 'toneE' ('toneD') functions, 'elemP' and 
-- 'elemCloseP', 'lengthP' and 'showD'.
data Params = P2 Int Int | P2s Int Int String | P3sf Int Int Int String | P4lsf Int Int Int [Int] String | P32sf Int Int Int String String 
 | P3lf Int Int [Int] deriving (Eq, Ord, Show)

-- | Is used to represent a set of durations parameters of the sounds and pauses. The positive value corresponds to the sound 
-- and the negative one -- to the pause.
type Durations = V.Vector Float

-- | Is used to represent a set of volumes in the amplitude scale for SoX \"vol\" effect.
type Strengths = V.Vector Float

-- | Is used to represent a set of intervals for notes (each element is a number of semi-tones between parts of interval). 
-- Positive values corresponds to lower notes and negative to higher ones.
type Intervals = V.Vector Int

-- | Additional function to produce signs from the given 'String' of the Ukrainian text. Ukrainian vowels and voiced consonants gives \"+\" sign (+1), voiceless
-- and sonorous consonants gives \"-\" sign (-1). Voiceless2 gives "0". Other symbols are not taken into account.
signsFromString :: Int -> String -> V.Vector Int
signsFromString n1 =
  V.take n1 . V.fromList . concatMap (fmap (\x -> if 
      | isVowel1 x -> 1
      | isVoicedC1 x -> 1
      | isVoicelessC1 x -> (-1)
      | isSonorous1 x -> (-1)
      | otherwise -> 0) . concatMap representProlonged) . createSyllablesUkrS . take (3 * n1) . cycle 
 
-- | Generalized version of the 'overSoXSynthGen2FDN_SG4G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthGen2FDN_SG4GPar :: FilePath -> Params -> (Float -> OvertonesO) -> Float -> Durations -> String -> 
  ((Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()) -> IO ()
overSoXSynthGen2FDN_SG4GPar file params f y v2 wws h = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
      zeroN = numVZeroesPre vecB in V.imapM_ (\j x -> do
        h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) j wws
        renameFile "result.wav" $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult  
   
-- | Generalized version of the 'overSoXSynthGen2FDN_SG6G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthGen2FDN_SG6GPar :: FilePath -> Params -> (Float -> OvertonesO) -> Float -> Durations -> String -> 
  ((Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()) -> Strengths -> Float -> IO ()
overSoXSynthGen2FDN_SG6GPar file params f y v2 wws h v6 limV
 | V.null v6 = putStrLn "You did not provide a volume adjustments vector! "
 | otherwise = do
    n <- duration1000 file
    vecA <- freqsFromFile file n
    let vecB = liftInParamsV params . V.map fromIntegral $ vecA
        zeroN = numVZeroesPre vecB in V.imapM_ (\j x -> do
          h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) j wws
          renameFile "result.wav" $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav"
          apply6GSilentFile ("result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") limV (V.unsafeIndex v6 (j `rem` V.length v6))) vecB
    endFromResult        

-- | Generalized version of the 'overSoXSynthGen2FDN_SG2G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthGen2FDN_SG2GPar :: FilePath -> Params -> (Float -> OvertonesO) -> Float -> String -> String -> ((Float -> OvertonesO) ->
  (Float, Float) -> Int -> String -> String -> IO ()) -> String -> IO ()
overSoXSynthGen2FDN_SG2GPar file params f y zs wws h ys = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j x -> do
        h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) j wws ys
        renameFile ("result." ++ if drop 3 ys == "f" then "flac" else "wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++
          if drop 3 ys == "f" then ".flac" else ".wav") vecB
  endFromResult2G ys    

-- | Generalized version of the 'overSoXSynthGen2FDN_Sf' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthGen2FDN_SfPar :: FilePath -> Params -> (Float -> OvertonesO) -> Float -> String -> String -> IO ()
overSoXSynthGen2FDN_SfPar file params f y zs wws = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j x -> do
        overSoXSynth2FDN_Sf f (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) j wws
        renameFile "result.wav" $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult    
    
-- | Generalized version of the 'overSoXSynthGen2FDN_Sf3G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthGen2FDN_Sf3GPar :: FilePath -> Params -> (Float -> OvertonesO) -> Float -> Float -> String -> String ->
 ((Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> IO ()) -> IO ()
overSoXSynthGen2FDN_Sf3GPar file params f y t0 zs wws h = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j x -> do
        h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2))), t0) j wws
        renameFile "result.wav" $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult  

-- | Generalized version of the 'overSoXSynthGen2FDN_Sf3G2G' where instead of lifting with 'liftInEnkuV' 'liftInParamsV' is used. It allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
overSoXSynthGen2FDN_Sf3G2GPar :: FilePath -> Params -> (Float -> OvertonesO) -> Float -> Float -> String -> String ->
 ((Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> String -> IO ()) -> String -> IO ()
overSoXSynthGen2FDN_Sf3G2GPar file params f y t0 zs wws h ys = do
  n <- duration1000 file
  vecA <- freqsFromFile file n
  let vecB = liftInParamsV params . V.map fromIntegral $ vecA
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j x -> do
        h f (x, (V.unsafeIndex v2 (j `rem` (V.length v2))), t0) j wws ys
        renameFile ("result." ++ if drop 3 ys == "f" then "flac" else "wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ if drop 3 ys == "f"
          then ".flac" else ".wav") vecB
  endFromResult2G ys    

-- | A way to get from a 'Params' a corresponding 'V.Vector' of 'Float' (if any) and so to work with them further. May contain some issues 
-- so please, before production usage check thoroughly. 
-- For information there were used the following: 
-- 
-- https://en.wikipedia.org/wiki/Mode_(music)
-- 
-- https://en.wikipedia.org/wiki/Ukrainian_Dorian_scale
-- 
-- https://en.wikipedia.org/wiki/List_of_musical_scales_and_modes
-- 
-- https://en.wikipedia.org/wiki/Octatonic_scale
-- 
-- several other articles in the English Wikipedia 
-- 
-- and in Ukrainian: 	
-- Смаглій Г., Маловик Л. Теорія музики : Підруч. для навч. закл. освіти, культури і мистецтв / Г.А. Смаглій. -- Х. : Вид-во \"Ранок\", 2013. -- 392 с. 
-- ISBN 978-617-09-1294-7
-- 
filterInParams :: Params -> Maybe (V.Vector Float)
filterInParams (P3lf n2 nL zs) -- generalized sound series, e. g. the chromatic ones etc.
 | all (\n -> compare n 0 /= LT) ([nL,107 - nL - n2,n2 - 2] ++ zs) = 
    if V.null . V.ifilter (\i _ -> getBFstLSorted' False (zip (sortNoDup . filter (< n2) $ zs) $ replicate n2 True) i) $ 
     (V.unsafeSlice nL n2 notes)
      then Nothing
      else Just (V.ifilter (\i _ -> getBFstLSorted' False (zip (sortNoDup . filter (< n2) $ zs) $ replicate n2 True) i) 
        (V.unsafeSlice nL n2 notes))
 | otherwise = Nothing
filterInParams (P32sf nT n2 nL xs ys) -- dur and moll in various their modifications
 | all (\n -> compare n 0 /= LT) [107 - nL - n2,nT,nL,nT - nL,nL + n2 - nT,n2 - 12] = 
    case xs of 
      "dur" -> getBFst' (Nothing,listArray (0,5) . zip ["FloatH","H","Full","Full moll","M","N"] $ fmap Just 
        [V.ifilter (\i _ -> toneD i nL nT [2,3,6,8,10]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneD i nL nT [1,3,5,9,10]) 
          (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> ((nL + i - nT + (((nT - nL) `quot` 12) + 1) * 12) `rem` 12) `notElem` [1,3,5]) 
            (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> ((nL + i - nT + (((nT - nL) `quot` 12) + 1) * 12) `rem` 12) `notElem` [1,6]) 
              (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneD i nL nT [1,3,5,9,11]) (V.unsafeSlice nL n2 notes), 
                V.ifilter (\i _ -> toneD i nL nT [1,3,5,8,10]) (V.unsafeSlice nL n2 notes)]) ys
      "moll" -> getBFst' (Nothing,listArray (0,5) . zip ["FloatH1","H","Full","Full dur","M","N"] $ fmap Just 
        [V.ifilter (\i _ -> toneD i nL nT [1,4,5,9,10]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneD i nL nT [1,4,6,9,10]) 
          (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> ((nL + i - nT + (((nT - nL) `quot` 12) + 1) * 12) `rem` 12) `notElem` [1,4,6]) 
            (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> ((nL + i - nT + (((nT - nL) `quot` 12) + 1) * 12) `rem` 12) `notElem` [1,6]) 
              (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneD i nL nT [1,4,6,8,10]) (V.unsafeSlice nL n2 notes), 
                V.ifilter (\i _ -> toneD i nL nT [1,4,6,9,11]) (V.unsafeSlice nL n2 notes)]) ys
      _   -> Nothing
 | otherwise = Nothing
filterInParams (P4lsf nT n2 nL zs xs) 
 | all (\n -> compare n 0 /= LT) ([107 - nL - n2,nT,nL,nT - nL,nL + n2 - nT,n2 - 2]  ++ zs) = 
    case xs of
     "ditonic" -> 
       if (V.length . V.ifilter (\i _ -> getBFstLSorted' False (zip (sortNoDup . filter (< n2) $ zs) $ replicate n2 True) 
        i) $ (V.unsafeSlice nL n2 notes)) /= 2 
         then Nothing
         else 
           if (V.unsafeIndex notes nT) `V.elem` (V.ifilter (\i _ -> getBFstLSorted' False (zip (take 2 . sortNoDup . filter (< n2) $ zs) $ 
            replicate n2 True) i) (V.unsafeSlice nL n2 notes))
             then Just (V.ifilter (\i _ -> getBFstLSorted' False (zip (take 2 . sortNoDup . filter (< n2) $ zs) $ replicate n2 True) 
               i) (V.unsafeSlice nL n2 notes))
             else Nothing
     "tritonic" -> 
       if (V.length . V.ifilter (\i _ -> getBFstLSorted' False (zip (sortNoDup . filter (< n2) $ zs) $ replicate n2 True) 
        i) $ (V.unsafeSlice nL n2 notes)) /= 3 
         then Nothing
         else 
           if (V.unsafeIndex notes nT) `V.elem` (V.ifilter (\i _ -> getBFstLSorted' False (zip (take 3 . sortNoDup . filter (< n2) $ zs) $ 
            replicate n2 True) i) (V.unsafeSlice nL n2 notes))
             then Just (V.ifilter (\i _ -> getBFstLSorted' False (zip (take 3 . sortNoDup . filter (< n2) $ zs) $ replicate n2 True) 
               i) (V.unsafeSlice nL n2 notes))
             else Nothing
     "tetratonic" -> 
       if (V.length . V.ifilter (\i _ -> getBFstLSorted' False (zip (sortNoDup . filter (< n2) $ zs) $ replicate n2 True) 
        i) $ (V.unsafeSlice nL n2 notes)) /= 4 
         then Nothing
         else 
           if (V.unsafeIndex notes nT) `V.elem` (V.ifilter (\i _ -> getBFstLSorted' False (zip (take 4 . sortNoDup . filter (< n2) $ zs) $ 
            replicate n2 True) i) (V.unsafeSlice nL n2 notes))
             then Just (V.ifilter (\i _ -> getBFstLSorted' False (zip (take 4 . sortNoDup . filter (< n2) $ zs) $ replicate n2 True) 
               i) (V.unsafeSlice nL n2 notes))
             else Nothing
     "octatonic" -> 
       if (V.length . V.ifilter (\i _ -> getBFstLSorted' False (zip (sortNoDup . filter (< n2) $ zs) $ replicate n2 True) 
        i) $ (V.unsafeSlice nL n2 notes)) /= 8 
         then Nothing
         else 
           if (V.unsafeIndex notes nT) `V.elem` (V.ifilter (\i _ -> getBFstLSorted' False (zip (take 8 . sortNoDup . filter (< n2) $ zs) $ 
            replicate n2 True) i) (V.unsafeSlice nL n2 notes))
             then Just (V.ifilter (\i _ -> getBFstLSorted' False (zip (take 8 . sortNoDup . filter (< n2) $ zs) $ replicate n2 True) 
               i) (V.unsafeSlice nL n2 notes))
             else Nothing
     _   -> Nothing
 | compare nL 0 /= LT && compare nL 107 /= GT && n2 == 1 && xs == "monotonic" = Just (V.singleton (V.unsafeIndex notes nL))
 | otherwise = Nothing
filterInParams (P2 nL n2) 
 | all (\n -> compare n 0 /= LT) [107 - nL - n2,nL,n2 - 2] = Just (V.unsafeSlice nL n2 notes) 
 | otherwise = Nothing
filterInParams (P2s nL n2 xs) 
 | all (\n -> compare n 0 /= LT) [107 - nL - n2,nL,n2 - 12] = 
    getBFstLSorted' Nothing (zip ["Egyptian pentatonic", "Prometheus hexatonic scale", "Ukrainian Dorian scale", "augmented hexatonic scale", 
      "blues major pentatonic", "blues minor pentatonic", "blues scale", "major hexatonic scale", "major pentatonic", "minor hexatonic scale", 
        "minor pentatonic", "tritone hexatonic scale", "two-semitone tritone hexatonic scale", "whole tone scale"] $ map Just 
          [V.ifilter (\i _ -> toneE i nL nL [0,2,5,7,10]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneE i nL nL [0,2,4,6,9,10]) (V.unsafeSlice nL n2 notes), 
            V.ifilter (\i _ -> toneE i nL nL [0,2,3,6,7,9,10]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneE i nL nL [0,3,4,7,8,11]) (V.unsafeSlice nL n2 notes), 
              V.ifilter (\i _ -> toneE i nL nL [0,2,5,7,9]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneE i nL nL [0,3,5,8,10]) (V.unsafeSlice nL n2 notes), 
                V.ifilter (\i _ -> toneE i nL nL [0,3,5,6,7,10]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneE i nL nL [0,3,5,6,7,10]) (V.unsafeSlice nL n2 notes), 
                  V.ifilter (\i _ -> toneE i nL nL [0,2,4,5,7,9]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneE i nL nL [0,2,4,7,9]) (V.unsafeSlice nL n2 notes), 
                    V.ifilter (\i _ -> toneE i nL nL [0,2,3,5,7,10]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneE i nL nL [0,3,5,7,10]) (V.unsafeSlice nL n2 notes), 
                      V.ifilter (\i _ -> toneE i nL nL [0,1,4,6,7,10]) (V.unsafeSlice nL n2 notes), V.ifilter (\i _ -> toneE i nL nL [0,1,3,7,8,9]) (V.unsafeSlice nL n2 notes), 
                        V.ifilter (\i _ -> toneE i nL nL [0,2,4,6,8,10]) (V.unsafeSlice nL n2 notes)]) xs
 | otherwise = Nothing
filterInParams (P3sf nT nL n2 xs) 
 | all (\n -> compare n 0 /= LT) [101 - nL,nT,nL,nT - nL,nL + 6 - nT] && n2 == 6 = 
    case xs of 
      "Dorian tetrachord" -> 
        if (nT - nL) `elem` [0,1,3,5] then Just (V.ifilter (\i _ -> toneE i nL nT [0,1,3,5]) (V.unsafeSlice nL 6 notes)) else Nothing
      "Phrygian tetrachord" -> 
        if (nT - nL) `elem` [0,2,3,5] then Just (V.ifilter (\i _ -> toneE i nL nT [0,2,3,5]) (V.unsafeSlice nL 6 notes)) else Nothing
      "Lydian tetrachord" -> 
        if (nT - nL) `elem` [0,2,4,5] then Just (V.ifilter (\i _ -> toneE i nL nT [0,2,4,5]) (V.unsafeSlice nL 6 notes)) else Nothing
      _   -> Nothing
 | all (\n -> compare n 0 /= LT) [94 - nL,nT,nL,nT - nL,nL + 13 - nT] && n2 == 13 = 
    getBFstLSorted' Nothing (zip ["modern Aeolian mode", "modern Dorian mode", "modern Ionian mode", "modern Locrian mode", 
      "modern Lydian mode", "modern Mixolydian mode", "modern Phrygian mode"] $ fmap (h3 nT n2 nL) [[1,4,6,9,11], [1,4,6,8,11], [1,3,6,8,10], 
        [2,4,7,9,11], [1,3,5,8,10], [1,3,6,8,11], [2,4,6,9,11]]) xs
 | otherwise = Nothing

h3 :: Int -> Int -> Int -> [Int] -> Maybe (V.Vector Float)
h3 nT n2 nL zs 
 | nT == nL = Just (V.ifilter (\i _ -> toneD i nL nT zs) (V.unsafeSlice nL n2 notes))
 | otherwise = Nothing

-- | For the list of @a@ from the @Ord@ class it builds a sorted in the ascending order list without duplicates.
-- 
-- > sortNoDup [2,1,4,5,6,78,7,7,5,4,3,2,5,4,2,4,54,3,5,65,4,3,54,56,43,5,2] = [1,2,3,4,5,6,7,43,54,56,65,78]
-- 
sortNoDup :: Ord a => [a] -> [a]
sortNoDup = sortNoDup' . sort
  where sortNoDup' (x:x1@(y:_)) 
         | x == y = sortNoDup' x1
         | otherwise = x:sortNoDup' x1
        sortNoDup' (x:_) = [x]
        sortNoDup' _ = []

-- | Checks whether its first 'Int' argument does not belong to those ones that are included into the list argument on the reminders basis. 
-- The opposite to 'toneE' with the same arguments.
toneD :: Int -> Int -> Int -> [Int] -> Bool
toneD i nL nT zs = ((nL + i - nT + (((nT - nL) `quot` 12) + 1) * 12) `rem` 12) `notElem` zs

-- | Checks whether its first 'Int' argument does belong to those ones that are included into the list argument on the reminders basis. 
-- The opposite to 'toneD' with the same arguments.
toneE :: Int -> Int -> Int -> [Int] -> Bool
toneE i nL nT zs = ((nL + i - nT + (((nT - nL) `quot` 12) + 1) * 12) `rem` 12) `elem` zs

-- | Analogous to 'liftInEnku' lifts a frequency into a tonality (or something that can be treated alike one) specified by 'Params'. If not 
-- reasonably one exists then the result is 11440 (Hz).
liftInParams :: Float -> Params -> Float
liftInParams x params 
 | lengthP params == 0 || (isNothing . whichOctaveG $ x) = 11440.0 
 | otherwise = 
    V.unsafeIndex (fromJust . filterInParams $ params) (V.minIndex . V.map (abs . log . (\t -> t / x)) . V.generate (lengthP params) $ 
      (\i -> V.unsafeIndex notes (12 * fromJust (whichOctaveG x)) * 2 ** (fromIntegral i / fromIntegral (lengthP params))))

-- | Application of the 'liftInParams' to a 'V.Vector'. 
liftInParamsV :: Params -> V.Vector Float -> V.Vector Float
liftInParamsV params = V.filter (/= 11440.0) . V.map (\x -> liftInParams x params)

-- | Gets a length of the 'V.Vector' of 'Float' being represented as 'Params'. This is a number of the notes contained in the 'Params'.
lengthP :: Params -> Int
lengthP = fromMaybe 0 . fmap V.length . filterInParams

-- | Check whether a given 'Float' value (frequency of a note) is in the vector of Floats that corresponds to the given 'Params'.
elemP :: Float -> Params -> Bool
elemP note = fromMaybe False . fmap (note `V.elem`) . filterInParams

-- | Check whether a given 'Float' value (frequency of the closest note to the given frequency) is in the vector of Floats that 
-- corresponds to the given 'Params'.
elemCloseP :: Float -> Params -> Bool
elemCloseP note = fromMaybe False . fmap (closestNote note `V.elem`) . filterInParams

-- | A way to show not the (somewhat algebraic) structure of the 'Params' (as the usual 'show' does), but the contained frequencies in it. 
showD :: Params -> String
showD = show . filterInParams 

-- | Check whether for the given arguments there are the notes and whether 'String' is a name signature for the scale in 'Params' (can they be used 
-- together to correspond to a non-empty set of notes).
isStrParams :: String -> Params -> Bool
isStrParams xs (P2s x y zs) = if isJust (filterInParams (P2s x y zs)) then xs == zs else False
isStrParams xs (P3sf x y z zs) = if isJust (filterInParams (P3sf x y z zs)) then xs == zs else False
isStrParams xs (P4lsf x y z ts zs) = if isJust (filterInParams (P4lsf x y z ts zs)) then xs == zs else False
isStrParams xs (P32sf x y z zs ys) = if isJust (filterInParams (P32sf x y z zs ys)) then (xs == zs || xs == ys || xs == (ys ++ " " ++ zs)) else False
isStrParams _ _ = False

-- | Check whether for the given arguments there are the notes and whether list of 'Int' is a part of the constructed 'Params' (can they be used 
-- together to correspond to a non-empty set of notes).
isListParams :: [Int] -> Params -> Bool
isListParams xs (P4lsf x y z ts zs) = if isJust (filterInParams (P4lsf x y z ts zs)) then xs == ts else False
isListParams xs (P3lf x y zs) = if isJust (filterInParams (P3lf x y zs)) then xs == zs else False
isListParams _ _ = False

-- | Generates melody for the given parameters. The idea is that every application of the function @f :: Float -> OvertonesO@ to its argument 
-- possibly can produce multiple overtones being represented as 'V.Vector' of tuples of pairs of 'Float'. We can use the first element in the 
-- tuple to obtain a new sound parameters and the second one -- to obtain its new duration in the melody. Additional function @g :: Float -> Float@ 
-- is used to avoid the effect of becoming less and less -- closer to the zero for the higher overtones so the durations will become also less. 
-- Besides it allows to rescale the durations in a much more convenient way. 
-- 
-- The first 'Float' parameter is a multiplication coefficient to increase or to decrease the durations (values with an absolute values greater than 
-- one correspond to increasing inside the @g@. function applied afterwards with function composition and the values with an absolute values less 
-- than one and not equal to zero correspond to decreasing inside the @g@ function. 
-- The second 'Float' parameter is a usual frequency which is used instead of the 11440.0 (Hz) value. 
-- The third 'Float' parameter is a main argument -- the frequency for which the 'OvertonesO' are generated as a first step of the computation. 
overMeloPar :: (Float -> OvertonesO) -> (Float -> Float) -> Params -> Float -> Float -> Float -> IO ()
overMeloPar f g params coeff freq0 freq = do 
  let v = f freq
      vFreqs = V.map ((\z -> if z == 11440.0 then freq0 else z) . flip liftInParams params . fst) v
      vD = V.map (g . (* coeff) . snd) v
      v2 = V.map f vFreqs
      vS = V.map (\z -> showFFloat (Just 4) (abs z) "") vD
      h42 j (x,v3,y,ts) 
        | compare y 0.0 == GT = do 
           (_,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",showFFloat Nothing (fst x) ""] ""
           print herr
           partialTest_k v3 0 ts
           mixTest
           renameFile "result.wav" $ "result" ++ prependZeroes (numVZeroesPre v) (show j) ++ ".wav"
        | compare y 0.0 == LT = do 
           (_,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "result.wav", "synth", ts,"sine",showFFloat Nothing (fst x) "","vol","0"] "" 
           putStr herr
           renameFile "result.wav" $ "result" ++ prependZeroes (numVZeroesPre v) (show j) ++ ".wav"
        | otherwise = putStrLn "Zero length of the sound! " 
  V.imapM_ (\j zz -> h42 j zz) . V.zip4 v v2 vD $ vS

-- | A default way to get 'Durations' for the sounds up to 0.35.2.0 version of the package including. It is based on the number of Ukrainian 
-- sounds representations (see, 'convertToProperUkrainian') in a Ukrainian syllables or somewhat generated by the same rules as they. 
-- The rhythm using the function is very often not binary but its ratios are almost always a ratios of the small natural numbers (1, 2, 3, 4, 5, 6, 7 etc.).
str2DurationsDef :: Int -> String -> Float -> Durations
str2DurationsDef n zs y = 
  let (t, ws) = splitAt 1 . syllableStr n $ zs in V.map (\yy -> y * fromIntegral (yy * length ws) / fromIntegral (head t)) . V.fromList $ ws 
  
apply6GSilentFile :: FilePath -> Float -> Float -> IO ()
apply6GSilentFile file limV vol = do
  upp <- upperBnd file
  ampL2 <- fmap ((\zz -> read zz::Float) . fst) (selMaxAbs file (0,upp))
  if compare (abs ampL2) (abs limV) /= GT then putStr ""
  else apply6Gf vol file

-- | Apply volume adjustment to the sound file. It must not be silent. Otherwise, it leads to likely noise sounding or errors.
apply6Gf :: Float -> FilePath -> IO ()
apply6Gf vol file = soxE file ["norm","vol", showFFloat (Just 4) vol ""]  

-- | Function is used to generate a rhythm of the resulting file \'end.wav\' from the Ukrainian text and a number of sounds either in the syllables or in the words without vowels.
syllableStr :: Int -> String -> [Int]
syllableStr n xs =
  let ps = map fromIntegral . take n . cycle . concatMap (map length) . createSyllablesUkrS $ xs
      y  = sum ps in
       case y of
         0 -> [0]
         _ -> y:ps  

-- | Similar to 'overSoXSynth2FDN_S' but additionally the program filters out from the resulting 'V.Vector' after \"f\" application values that are smaller
-- by absolute value than 0.001. An 'Int' parameter is used to define an interval. To obtain compatible with versions prior to
-- 0.20.0.0 behaviour, use for the 'Int' 0.
--
-- Be aware that the result can be rather unpredictable (the program can even obtain segmentation fault) for not very suitable function.
-- But for a lot of functions this works well.
-- 
-- It is recommended to fully simplify the computation for \"f\" function before using it in the 'overSoXSynth2FDN_Sf'.
overSoXSynth2FDN_Sf :: (Float -> OvertonesO) -> (Float, Float) -> Int -> String -> IO ()
overSoXSynth2FDN_Sf f (x, y) = overSoXSynth2FDN_Sf3 f (x, y, 0.001)

-- | Similar to 'overSoXSynth2FDN_S' but additionally the program filters out from the resulting 'V.Vector' after \"f\" application values that are smaller
-- than the third 'Float' parameter by an absolute value in the triple of @Float@'s. An 'Int' parameter is used to define an interval. To obtain compatible
-- with versions prior to 0.20.0.0 behaviour, use for the 'Int' 0.
--
-- Be aware that the result can be rather unpredictable (the program can even obtain segmentation fault) for not very suitable function.
-- But for a lot of functions this works well.
-- 
-- It is recommended to fully simplify the computation for \"f\" function before using it in the 'overSoXSynth2FDN_Sf3'.
overSoXSynth2FDN_Sf3 :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> IO ()
overSoXSynth2FDN_Sf3 f (x, y, t0) j zs = overSoXSynth2FDN_Sf32G f (x, y, t0) j zs V.empty []

-- | Generalized variant of the 'overSoXSynth2FDN_Sf31G' with a possibility to specify sound quality using the second 'String' parameter.
-- For more information, please, refer to 'soxBasicParams'.
overSoXSynth2FDN_Sf32G :: (Float -> OvertonesO) -> (Float, Float, Float) -> Int -> String -> V.Vector Float -> String -> IO ()
overSoXSynth2FDN_Sf32G f (x, y, t0) j zs vdB ys
 | null . convertToProperUkrainianI8 $ zs = overSoXSynth x
 | otherwise = do
    let l0    = length zs
    soundGenF32G (V.fromList [\x2 -> closestNote (if x2 /= 0.0 then abs x2 else V.unsafeIndex notes 0),\x2 -> fromMaybe (V.unsafeIndex notes 0)
     (dNote (V.unsafeIndex (intervalsFromString zs) (abs (j `rem` l0))) (closestNote (if x2 /= 0.0 then abs x2 else V.unsafeIndex notes 0)))])
       (V.replicate 2 x) (V.fromList [1,V.unsafeIndex (intervalsFromString zs) (abs (j `rem` l0))]) f (x, y, t0) j vdB ys
    if null ys then mixTest else mixTest2G ys

-- | Function is used to get numbers of intervals from a Ukrainian 'String'. It is used internally in the 'uniqOverSoXSynthN4' function.
intervalsFromString :: String -> Intervals
intervalsFromString = vStrToVIntG defInt . newRepresentation . convertToProperUkrainianI8

-- | Generatlized version of the 'vStrToVInt' with a possibility to specify your own 'Intervals'.
vStrToVIntG :: Intervals -> V.Vector String -> Intervals
vStrToVIntG v = V.map (strToIntG v)

-- | Default values for 'strToInt'. All the intervals are not greater than one full octave.
defInt :: Intervals
defInt = V.fromList [12,4,7,3,4,5,5,12,3,8,12,7,10,7,7,7,12,10,7,10,2,12,2,2,11,11,1,12,9]
{-# INLINE defInt #-}

-- | Generatlized version of the 'strToInt' with a possibility to specify your own 'Intervals'.
strToIntG :: Intervals -> String -> Int
strToIntG v =
  getBFstLSorted' 0 (zip (["а","б","в","г","д","дж","дз","е","ж","з","и","й","к","л","м","н","о","п","р","с","т","у","ф","х","ц","ч","ш","і","ґ"]) . V.toList $ v)
{-# INLINE strToIntG #-}

-- | Generalized variant of the 'soundGenF31G' with a possibility to specify sound quality using the 'String' argument. For more information,
-- please, refer to 'soxBasicParams'.
soundGenF32G :: V.Vector (Float -> Float) -> V.Vector Float -> V.Vector Int -> (Float -> OvertonesO) -> (Float, Float, Float) -> Int ->
  V.Vector Float -> String -> IO ()
soundGenF32G vf vd vi f (x, y, t0) j vdB ys = do
  let vD = helpF1 vf vd vi   -- Vector of notes played simultaneously (e. g. just one, interval, accord etc.)
      vDz = V.mapMaybe id vD -- The previous one without Nothings and Justs
      vNotes = doubleVecFromVecOfFloat f t0 (V.map Just vDz) -- Vector of vectors of pairs (freq,ampl) -- notes and their absence (V.empty) with overtones
      ts = showFFloat (Just 4) (abs y) "" -- duration of the sound to be generated
  V.imapM_ (\i _ -> do
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ((if V.null vdB 
       then id else (\wwws -> adjust_dbVol wwws (V.unsafeIndex vdB i))) ["-r22050", "-n", "test" ++ helpF0 i ++ ".wav", "synth",ts,
         "sine", showFFloat Nothing (V.unsafeIndex vDz i) "","vol", if compare y 0.0 == GT then "1.0" else "0"])) ""
    partialTest_k2G (V.unsafeIndex vNotes i) i ts vdB ys) vDz      

helpF0 :: Int -> String
helpF0 =
  getBFstLSorted' "ZZ0" (zip [0..] $ (map (:[]) "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++ concatMap (\z -> map ((z:) . (:[])) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) 

helpF1 :: V.Vector (Float -> Float) -> V.Vector Float -> V.Vector Int -> V.Vector (Maybe Float)
helpF1 vf vd =
  V.map (\(f1,x,i2) ->
    case i2 of
      0 -> Nothing
      _ -> Just $ f1 x) . V.zip3 vf vd

-- | Generates a 'V.Vector' of 'OvertonesO' that represents the sound. 
doubleVecFromVecOfFloat :: (Float -> OvertonesO) -> Float -> V.Vector (Maybe Float) -> V.Vector OvertonesO
doubleVecFromVecOfFloat f t0 =
  V.map (\note1 -> if isNothing note1 then V.empty else V.filter (\(_,!z) -> compare (abs z) t0 == GT) . f . fromJust $ note1)          
