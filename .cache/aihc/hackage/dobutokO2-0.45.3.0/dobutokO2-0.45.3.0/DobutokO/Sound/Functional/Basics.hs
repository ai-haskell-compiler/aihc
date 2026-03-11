-- |
-- Module      :  DobutokO.Sound.Functional.Basics
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside. Is more complicated than
-- dobutokO2 and uses its functionality.

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Functional.Basics (
  -- * Type synonyms with different semantics
  SoundsO
  , OvertonesO
  , NotePairs
  -- * Work with notes (general)
  , notes
  , neighbourNotes
  , closestNote
  , pureQuintNote 
  , overTones 
  -- * Work with overtones
  , overSoXSynth 
  -- * Work with enky (extension to octaves functionality)
  , nkyT
  , whichOctave
  , whichOctaveG
  , whichEnka 
  , enkuUp 
  , enkuDown 
  , liftInEnkuV
  , liftInEnku
  -- ** Work with octaves
  , octavesT
  -- * Combining intermediate files
  , mixTest
  , mixTest2
  -- * Working with files
  , freqsFromFile
  , endFromResult
  -- * Use additional function and Ukrainian texts and generates melody
  , dNote
  -- ** 2G generalized auxiliary functions
  , mixTest2G
  , mixTest22G
  , endFromResult2G
  -- ** Auxiliary functions
  , partialTest_k
  , partialTest_k1G
  , partialTest_k2G
  , prependZeroes 
  , nOfZeroesLog 
  , numVZeroesPre
  , duration1000
  , adjust_dbVol
) where

import GHC.Arr
import CaseBi.Arr (getBFst')
import Data.Char (isDigit)
import System.Exit (ExitCode( ExitSuccess ))
import Numeric
import Data.List (isPrefixOf,sort)
import Data.Maybe (fromJust,isJust,fromMaybe)
import qualified Data.Vector as V
import Sound.SoXBasics (durationA)
import System.Process
import EndOfExe2
import System.Directory
import DobutokO.Sound.IntermediateF

-- | Is used to represent a sequence of intervals, each note being a 'Float' value (its frequency in Hz).
type SoundsO = V.Vector (Float, Float)

-- | Is used to represent a set of overtones for the single sound, the first 'Float' value is a frequency and the second one -- an amplitude.
type OvertonesO = V.Vector (Float, Float)

-- | Is used to represent a set of pairs of notes for each element of which the 'Float' values (notes frequencies in Hz) are somewhat
-- musically connected one with another..
type NotePairs = V.Vector (Float, Float)

-- | Gets 'V.Vector' of 'Int' frequencies from the given 'FilePath' using SoX. The frequencies are \"rough\" according to the SoX documentation and
-- the duration is too small so they can be definitely other than expected ones. Is used as a source of variable numbers (somewhat close each to another
-- in their order but not neccessarily). .
freqsFromFile :: FilePath -> Int -> IO (V.Vector Int)
freqsFromFile file n = V.generateM n (\k -> do {
    (_, _, herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "trim", showFFloat Nothing (fromIntegral k * 0.001) "",
      "0.001", "stat"] ""
    ; let line0s = lines herr
          noteN0 = takeWhile isDigit . dropWhile (not . isDigit) . concat . drop 13 . take 14 $ line0s
    ; if null noteN0 then return (11440::Int)
      else let noteN1  = read (takeWhile isDigit . dropWhile (not . isDigit) . concat . drop 13 . take 14 $ line0s)::Int in return noteN1 })

-- | Combines (mixes) all \"test\*" files in the given directory. The files should be similar in parameters and must be sound files for SoX to work
-- on them properly. Afterwards, the function deletes these combined files.
mixTest :: IO ()
mixTest = do
  paths0 <- listDirectory "."
  let paths = filter (isPrefixOf "test") $ paths0
  _ <- readProcessWithExitCode (fromJust (showE "sox")) (["--combine", "mix"] ++ paths ++ ["result.wav","vol","0.3"]) ""
  mapM_ removeFile paths

-- | Similar to 'mixTest', but allows to change the sound quality parameters for the resulting file. For more information, please, refer to
-- 'soxBasicParams'.
mixTest2G :: String -> IO ()
mixTest2G ys = do
  paths0 <- listDirectory "."
  let paths = filter (isPrefixOf "test") $ paths0
  _ <- readProcessWithExitCode (fromJust (showE "sox")) (["--combine", "mix"] ++ paths ++ soxBasicParams ys ["","result.wav","vol","0.3"]) ""
  mapM_ removeFile paths

-- | Combines (mixes) all \"test\*" files in the given directory. The files should be similar in parameters and must be sound files for SoX to work
-- on them properly. Afterwards, the function deletes these combined files. The name of the resulting file depends on the first two command line
-- arguments so that it is easy to produce unique names for the consequent call for the function.
mixTest2 :: Int -> Int -> IO ()
mixTest2 zeroN j = do
  paths0 <- listDirectory "."
  let paths = filter (isPrefixOf "test") $ paths0
  _ <- readProcessWithExitCode (fromJust (showE "sox")) (["--combine", "mix"] ++ paths ++ ["result" ++ prependZeroes zeroN (show j) ++ ".wav",
    "vol","0.3"]) ""
  mapM_ removeFile paths

-- | Similar to 'mixTest', but allows to change the sound quality parameters for the resulting file. For more information, please, refer to
-- 'soxBasicParams'. The name of the resulting file depends on the first two command line
-- arguments so that it is easy to produce unique names for the consequent call for the function.
mixTest22G :: Int -> Int -> String -> IO ()
mixTest22G zeroN j ys = do
  paths0 <- listDirectory "."
  let paths = filter (isPrefixOf "test") $ paths0
  _ <- readProcessWithExitCode (fromJust (showE "sox")) (["--combine", "mix"] ++ paths ++ soxBasicParams ys ["","result" ++ prependZeroes zeroN (show j) ++
      ".wav","vol","0.3"]) ""
  mapM_ removeFile paths  

-- | Gets an \"end.wav\" file from the intermediate \"result\*.wav\" files in the current directory. If it is not successful, produces the notification
-- message and exits without error. If you would like to create the file if there are too many intermediate ones, please, run
-- \"dobutokO2 8\" or \"dobutokO2 80\" in the current directory.
endFromResult :: IO ()
endFromResult = do
  path2s <- listDirectory "."
  let paths3 = sort . filter (isPrefixOf "result") $ path2s
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) (paths3 ++ ["end.wav"]) ""
  case code of
    ExitSuccess -> putStrLn "The final file \"end.wav\" was successfully created. You can now manually change or delete \"result*\" files in the directory. "
    _           -> do
      exi <- doesFileExist "end.wav"
      if exi then removeFile "end.wav"
      else putStr "Your final file \"end.wav\" was not created by some reason, but the intermediate files in the respective order are present. " >>
        putStrLn "Use them manually as needed."

-- | Similar to 'endFromResult', but uses additional 'String' argument to change sound quality parameters. For more information, please, refer to
-- 'soxBasicParams'.
endFromResult2G :: String -> IO ()
endFromResult2G ys = do
  path2s <- listDirectory "."
  let paths3 = sort . filter (isPrefixOf "result") $ path2s
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) (paths3 ++ soxBasicParams ys ["","end.wav"]) ""
  case code of
    ExitSuccess -> putStrLn $ "The final file \"end." ++ if drop 3 ys == "f" then "flac" else "wav" ++ "\" was successfully created. You can now manually change or delete \"result*\" files in the directory. "
    _           -> do
      exi <- doesFileExist $ "end." ++ if drop 3 ys == "f" then "flac" else "wav"
      if exi then removeFile $ "end." ++ if drop 3 ys == "f" then "flac" else "wav"
      else putStr "Your final file \"end.wav\" was not created by some reason, but the intermediate files in the respective order are present. " >>
        putStrLn "Use them manually as needed."            

-- | Creates part of the needed \"test\*\.wav" files in the current directory. 
partialTest_k :: OvertonesO -> Int -> String -> IO ()
partialTest_k vec k ts = partialTest_k2G vec k ts V.empty []

-- | Generalized version of the 'partialTest_k' with the additional volume adjustment in dB given by 'V.Vector' of 'Float'.
partialTest_k1G :: OvertonesO -> Int -> String -> V.Vector Float -> IO ()
partialTest_k1G vec k ts vdB = partialTest_k2G vec k ts vdB []

-- | Generalized version of the 'partialTest_k1G' with a possibility to change sound quality parameters using the additional second 'String' argument.
-- For more information, please, refer to 'soxBasicParams'.
partialTest_k2G :: OvertonesO -> Int -> String -> V.Vector Float -> String -> IO ()
partialTest_k2G vec k ts vdB ys =
  let zeroN = numVZeroesPre vec in V.imapM_ (\i (noteN, !amplN) -> if i /= 0 && i `rem` 50 == 0
    then do
      _ <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys (adjust_dbVol ["-r22050", "-n", "test" ++ show k ++ show (i + 2) ++ ".wav",
         "synth", ts,"sine", showFFloat Nothing (abs noteN) "", "vol", showFFloat Nothing amplN ""]
            (V.unsafeIndex vdB i))) ""
      path1s <- listDirectory "."
      let path2s = sort . filter (isPrefixOf $ "test" ++ show k) $ path1s
      (code,_,herr0) <- readProcessWithExitCode (fromJust (showE "sox")) (["--combine", "mix"] ++ path2s ++ soxBasicParams ys ["","test-" ++ show k ++
        prependZeroes zeroN (show (i `quot` 50)) ++ ".wav"]) ""
      case code of
        ExitSuccess -> mapM_ removeFile path2s
        _           -> do
          exi <- doesFileExist $ "test-" ++ show k ++ prependZeroes zeroN (show (i `quot` 50)) ++ if drop 3 ys == "f" then ".flac" else ".wav"
          if exi then putStrLn (herr0) >> removeFile ("test-" ++ show k ++ prependZeroes zeroN (show (i `quot` 50)) ++ if drop 3 ys == "f" then ".flac" else ".wav")
          else putStrLn herr0
    else readProcessWithExitCode (fromJust (showE "sox")) ((if null ys then id else soxBasicParams ys) ((if V.null vdB then id 
      else (\wwws -> adjust_dbVol wwws (V.unsafeIndex vdB i))) ["-r22050", "-n", "test" ++ show k ++ show (i + 2) ++ ".wav",
       "synth", ts,"sine", showFFloat Nothing (abs noteN) "", "vol", showFFloat Nothing amplN ""])) "" >> putStr "") vec

-- | Auxiliary function to get from a sound file specified a duration parameter @n@ that can be used further.
duration1000 :: FilePath -> IO Int
duration1000 file = fmap (\t -> truncate (t / 0.001)) . durationA $ file

-- | Function to get from the number of semi-tones and a note a 'Maybe' note for the second lower note in the interval if any. If there is
-- no need to obtain such a note, then the result is 'Nothing'.
dNote :: Int -> Float -> Maybe Float
dNote n note
  | n == 0 || compare note (V.unsafeIndex notes 0) == LT || compare note (V.unsafeIndex notes 107) == GT = Nothing
  | otherwise = Just (note / 2 ** (fromIntegral n / 12))

-- | 'V.Vector' of musical notes in Hz.
notes :: V.Vector Float
-- notes V.! 57 = 440.0   -- A4 in Hz
notes = V.generate 108 (\t ->  440 * 2 ** (fromIntegral (t - 57) / 12))

-- | Function returns either the nearest two musical notes if frequency is higher than one for C0 and lower than one for B8
-- or the nearest note duplicated in a tuple.
neighbourNotes :: Float -> V.Vector Float -> (Float, Float)
neighbourNotes x v
  | compare x (V.unsafeIndex v 0) /= GT = (V.unsafeIndex v 0, V.unsafeIndex v 0)
  | compare x (V.unsafeIndex v (V.length v - 1)) /= LT = (V.unsafeIndex v (V.length v - 1), V.unsafeIndex v (V.length v - 1))
  | compare (V.length v) 2 == GT = if compare x (V.unsafeIndex  v (V.length v `quot` 2)) /= GT
      then neighbourNotes x (V.unsafeSlice 0 (V.length v `quot` 2 + 1) v)
      else neighbourNotes x (V.unsafeSlice (V.length v `quot` 2) (V.length v - (V.length v `quot` 2)) v)
  | otherwise = (V.unsafeIndex v 0, V.unsafeIndex v (V.length v - 1))

-- | Returns the closest note to the given frequency in Hz.  
closestNote :: Float -> Float
closestNote x
 | compare x 0.0 == GT =
    let (x0, x2) = neighbourNotes x notes
        r0       = x / x0
        r2       = x2 / x in 
     if compare r2 r0 == GT
       then x0
       else x2
 | otherwise = 0.0

-- | Additional function to prepend zeroes to the given 'String'. The number of them are just that one to fulfill the length to the given 'Int' parameter.
prependZeroes :: Int -> String -> String 
prependZeroes n xs 
  | if compare n 0 /= GT || null xs then True else compare n (length xs) /= GT = xs
  | otherwise = replicate (n - length xs) '0' ++ xs
{-# INLINE prependZeroes #-}  

nOfZeroesLog :: Int -> Maybe Int
nOfZeroesLog x
  | compare x 0 /= GT = Nothing
  | otherwise = Just (truncate (logBase 10 (fromIntegral x)) + 1)
{-# INLINE nOfZeroesLog #-}  

-- | Is a minimal number of decimal places that are just enough to represent a length of the 'V.Vector' given. For an 'V.empty' returns 0.
numVZeroesPre :: V.Vector a -> Int
numVZeroesPre v = fromMaybe (0 :: Int) (nOfZeroesLog . V.length $ v)
{-# INLINE numVZeroesPre #-}      

-- | Similarly to 'liftInOctaveV' returns a 'V.Vector' 'Float' (actually frequencies) for the n-th elements set of notes (see 'nkyT') instead of octaves.
-- A second 'Int' parameter defines that @n@. 
liftInEnkuV :: Int -> Int -> V.Vector Float -> V.Vector Float
liftInEnkuV n ku = V.mapMaybe (liftInEnku n ku)

-- | Similarly to 'liftInOctave' returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT').
-- A second 'Int' parameter defines that @n@. Not all pairs return 'Just' @x@. 
liftInEnku :: Int -> Int -> Float -> Maybe Float
liftInEnku n ku x
  | compare n 0 == LT || compare n ((108 `quot` ku) - 1) == GT = Nothing
  | getBFst' (False, listArray (0,5) . zip [2,3,4,6,9,12] $ repeat True) ku && compare (closestNote x) 24.4996 == GT =
      case compare (fromJust . whichEnka ku $ x) n of
        EQ -> Just (closestNote x)
        LT -> let z  = logBase 2.0 (V.unsafeIndex notes (n * ku) / closestNote x)
                  z1 = truncate z in
                   if abs (z - fromIntegral z1) > 0.999 || abs (z - fromIntegral z1) < 0.001
                     then Just (V.unsafeLast . V.iterateN (fromIntegral z1 + 1) (enkuUp ku) $ closestNote x)
                     else Just (V.unsafeLast . V.iterateN (fromIntegral z1 + 2) (enkuUp ku) $ closestNote x)
        _  -> let z  = logBase 2.0 (closestNote x / V.unsafeIndex notes (n * ku))
                  z1 = truncate z in
                   if abs (z - fromIntegral z1) > 0.999 || abs (z - fromIntegral z1) < 0.001
                     then Just (V.unsafeLast . V.iterateN (fromIntegral z1 + 2) (enkuDown ku) $ closestNote x)
                     else Just (V.unsafeLast . V.iterateN (fromIntegral z1 + 1) (enkuDown ku) $ closestNote x)
  | otherwise = Nothing

-- | Similarly to 'whichOctave' returns a 'Maybe' number (actually frequency) for the n-th elements set of notes (see 'nkyT').
-- An 'Int' parameter defines that @n@.
whichEnka :: Int -> Float -> Maybe Int
whichEnka n x
  | getBFst' (False,listArray (0,5) . zip [2,3,4,6,9,12] $ repeat True) n && compare (closestNote x) 24.4996 == GT = (\t ->
     case isJust t of 
       True -> fmap (\z ->
         case z of
           0 -> z
           _ -> z - 1) t
       _    -> Just ((108 `quot` n) - 1)) . V.findIndex (\(t1, _) -> compare (closestNote x) t1 == LT) . nkyT $ n
  | otherwise = Nothing

-- | Returns an analogous note in the higher n-th elements set (its frequency in Hz) (see 'nkyT'). An 'Int' parameter defines this @n@.
enkuUp  :: Int -> Float -> Float
enkuUp n x
  | getBFst' (False, listArray (0,9) . zip [2..11] $ repeat True) n = 2 ** (fromIntegral n / 12) * x
  | otherwise = 2 * x
{-# INLINE enkuUp #-}  

-- | Returns an analogous note in the lower n-th elements set (its frequency in Hz) (see 'nkyT'). An 'Int' parameter defines this @n@.
enkuDown  :: Int -> Float -> Float
enkuDown n x
  | getBFst' (False, listArray (0,9) . zip [2..11] $ repeat True) n = 2 ** (fromIntegral (-n) / 12) * x
  | otherwise = x / 2
{-# INLINE enkuDown #-}

-- | Returns a 'V.Vector' of tuples with the lowest and highest frequencies for the notes in the sets consisting of @n@ consequential notes
-- (including semi-tones). An 'Int' parameter defines this @n@. It can be 2, 3, 4, 6, 9, or 12 (the last one is for default octaves, see 'octavesT').
-- So for different valid @n@ you obtain doubles, triples and so on. The function being applied returns a 'V.Vector' of such sets with
-- their respective lowest and highest frequencies.
nkyT :: Int -> NotePairs
nkyT n
  | getBFst' (False,listArray (0,5) . zip [2,3,4,6,9,12] $ repeat True) n = V.generate (108 `quot` n) (\i -> (V.unsafeIndex notes (i * n),
       V.unsafeIndex notes (i * n + (n - 1))))
  | otherwise = octavesT

-- | Returns a 'V.Vector' of tuples with the lowest and highest frequencies for the notes in the octaves.
octavesT :: NotePairs
octavesT = V.generate 9 (\i -> (V.unsafeIndex notes (i * 12), V.unsafeIndex notes (i * 12 + 11)))

-- | For the given frequency it generates a musical sound with a timbre. The main component of the sound includes the lower pure quint,
-- which can be in the same octave or in the one with the number lower by one. Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" and \"end.wav\" files in the current directory, because they can be overwritten.
overSoXSynth :: Float -> IO ()
overSoXSynth x = do
  let note0 = if x /= 0.0 then closestNote (abs x) else V.unsafeIndex notes 0
      note1 = pureQuintNote note0
      v0    = overTones note0
      v1    = overTones note1
      overSoXSynthHelp = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
        ["-r22050", "-n", "test0" ++ show (i + 2) ++ ".wav", "synth", "0.5","sine", showFFloat Nothing noteN "", "vol", showFFloat Nothing amplN ""] "")
      overSoXSynthHelp2 = V.imapM_ (\i (noteN, !amplN) -> readProcessWithExitCode (fromJust (showE "sox"))
        ["-r22050", "-n", "test1" ++ show (i + 2) ++ ".wav", "synth", "0.5","sine", showFFloat Nothing noteN "", "vol", showFFloat Nothing amplN ""] "")
  _ <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "test01.wav", "synth", "0.5","sine", showFFloat Nothing note0 "", "synth", "0.5","sine", "mix", showFFloat Nothing note1 "", "vol","0.5"] ""
  overSoXSynthHelp v0
  overSoXSynthHelp2 v1
  mixTest 

-- | Returns a pure quint lower than the given note.
pureQuintNote :: Float -> Float
pureQuintNote x = x / 2 ** (7 / 12)
{-# INLINE pureQuintNote #-}

-- | For the given frequency of the note it generates a 'V.Vector' of the tuples, each one of which contains the harmonics' frequency and amplitude.
overTones :: Float -> OvertonesO
overTones note =
  V.takeWhile (\(!w,!z) -> compare w (V.unsafeIndex notes 107) /= GT && compare (abs z) 0.001 == GT) . V.zip (V.generate 1024 (\i ->
    note * fromIntegral (i + 2))) $ (V.generate 1024 (\i -> 1 / fromIntegral ((i + 1) * (i + 1))))

-- | Function can be used to determine to which octave (in the American notation for the notes, this is a number in the note written form,
-- e. g. for C4 this is 4) the frequency belongs (to be more exact, the closest note for the given frequency -- see 'closestNote' taking into account
-- its lower pure quint, which can lay in the lower by 1 octave). If it is not practical to determine the number, then the function returns 'Nothing'.
whichOctave :: Float -> Maybe Int
whichOctave x
  | compare (closestNote x) 24.4996 == GT = (\t ->
     case isJust t of 
       True -> fmap (\z ->
         case z of
           0 -> z
           _ -> z - 1) t
       _    -> Just 8) . V.findIndex (\(t1, _) -> compare (closestNote x) t1 == LT) $ octavesT
  | otherwise = Nothing

-- | Generalized version of the 'whichOctave'.
whichOctaveG :: Float -> Maybe Int
whichOctaveG x 
  | compare (closestNote x) (V.unsafeIndex notes 0) == GT && compare x (V.unsafeIndex notes 107) /= GT = (\t ->
     case isJust t of 
       True -> fmap (\z ->
         case z of
           0 -> z
           _ -> z - 1) t
       _    -> Just 8) . V.findIndex (\(t1, _) -> compare (closestNote x) t1 == LT) $ octavesT
  | otherwise = Nothing

-- | Is used internally in the 'readProcessWithExitCode' to adjust volume for the sound with additional dB value given by 'Float' argument.
adjust_dbVol :: [String] -> Float -> [String]
adjust_dbVol xss y
 | y == 0.0 = xss
 | otherwise = xss ++ ["vol",showFFloat Nothing y "dB"]

 
