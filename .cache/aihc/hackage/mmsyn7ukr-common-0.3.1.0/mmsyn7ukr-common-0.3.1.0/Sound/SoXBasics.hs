-- {-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Sound.SoXBasics
-- Copyright   :  (c) OleksandrZhabenko 2019-2021, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr.zhabenko@yahoo.com
--
-- A program and a library that can be used as a simple basic interface to some SoX functionality.
--


module Sound.SoXBasics (
  -- * Encoding file extensions and types functional data type
  ULencode(..)
  , SoundFileExts(..)
  -- * Working with two extensions
  , soxOpG, soxOpG1, ulAccessParameters, ulResultParameters
  -- * Some generalized common functions
  , doubleCleanCheck, presenseCheck, secondFileClean, twoIntermediateFs, twoExceptions1File, applyExts2, beforeExtStr
  -- * Get Information
  , maxAbs, getMaxAG, getMinAG, selMaxAbsG, selMAG, extremeSG, extremeSG1G, soxStatG, upperBndG, durationAG, sampleAnG
  -- * Produce sound
  -- ** Trimming the silence
  , alterVadBG, alterVadEG, alterVadHelpG, opFileG
  -- ** Amplitude modification
  , normG, normLG, gainLG, quarterSinFadeG
  -- ** Adding silence
  , silenceBothG
  -- ** Changing sample rate
  , resampleAG
  -- ** Working with noise
  , noiseProfBG, noiseProfEG, noiseReduceBG, noiseReduceEG, noiseReduceBUG, noiseReduceEUG
  -- ** Filtering
  , sincAG
  -- ** Volume amplification
  , volSG, volS2G
  -- * Variants that uses just .wav files
  , getMaxA, getMinA, selMaxAbs, selMA, extremeS, extremeS1, soxStat, upperBnd, durationA, sampleAn
  , alterVadB, alterVadE, alterVadHelp, opFile
  , norm, normL, gainL, quarterSinFade
  , silenceBoth
  , resampleA
  , noiseProfB, noiseProfE, noiseReduceB, noiseReduceE, noiseReduceBU, noiseReduceEU
  , sincA
  , volS, volS2
  -- * Playback
  , playA
) where

import System.Directory
import Data.Maybe (isJust, fromJust)
import Data.List (isSuffixOf)
import Numeric
import Data.Char
import System.Process
import System.IO
import EndOfExe2
import System.Exit
import Control.Concurrent (threadDelay)
import Control.Exception (onException)
import System.Info (os)
import Sound.Control.Exception.FinalException
import GHC.Base (mconcat)

-- | Function 'maxAbs' allows to choose a maximum by absolute value if the values are written as 'String'. Bool 'True' corresponds to maximum value, 'False' - to minimum value
maxAbs :: (String, String) -> (String, Bool)
maxAbs (xs, ys) | null xs || null ys = ([], False)
                | hx == "-" && hy == "-" = if compare xs ys /= LT then (xs, False) else (ys, False)
                | hx /= "-" && hy /= "-" = if compare xs ys == GT then (xs, True) else (ys, True)
                | hx == "-" && hy /= "-" = if compare (drop 1 xs) ys /= LT then (xs, False) else (ys, True)
                | otherwise = if compare xs (drop 1 ys) == GT then (xs, True) else (ys, False)
            where hx = take 1 xs
                  hy = take 1 ys

ulAccessParameters :: [String]
ulAccessParameters = ["-r22050","-c1"]

ulResultParameters :: [String]
ulResultParameters = ["-r22050","-c1"]

data ULencode = W | UL1 | UL0 | UL deriving (Eq, Ord)

instance Show ULencode where
  show W = "(False, False)" -- Only working with .wav files.
  show UL1 = "(False, True)" -- .ul appears.
  show UL0 = "(True, False)" -- .ul disappears.
  show _ = "(True, True)" -- .ul is constantly used.

class SoundFileExts a where
  getExts :: a -> (String,String)
  isFileExtsR :: a -> FilePath -> FilePath -> Bool
  isFileExtsR ul file1 file2 = xs `isSuffixOf` file1 && ys `isSuffixOf` file2
    where (xs,ys) = getExts ul

instance SoundFileExts ULencode where
  getExts W = (".wav",".wav")
  getExts UL1 = (".wav",".ul")
  getExts UL0 = (".ul",".wav")
  getExts _ = (".ul",".ul")

-- | Is partially defined, is used internally here.
applyExts2 :: ULencode -> FilePath -> FilePath -> (FilePath, FilePath)
applyExts2 ul file1 file2 = (beforeExtStr file1 ++ xs, beforeExtStr file2 ++ ys)
  where (xs,ys) = getExts ul

beforeExtStr :: FilePath -> String
beforeExtStr file =
 case end of
  ".wav" -> begin
  (z:".ul") -> begin ++ [z]
  _  -> error "Sound.SoXBasics.beforeExtStr: The file has neither .wav, nor .ul extension."
  where l = length file - 4
        (begin,end) = splitAt l file

-- | The 'FilePath' cannot be \"-n\", please, use in such a case a more convinient function 'soxOpG1'.
soxOpG :: ULencode -> [String] -> FilePath -> [String] -> FilePath -> [String] -> IO (ExitCode, String, String)
soxOpG ul xss file1 yss file2 zss
 | isFileExtsR ul file1 file2 = readProcessWithExitCode (fromJust (showE "sox")) (filter (not . null) . mconcat $
  case ul of { W -> [xss, [file10], yss, [file20], zss] ; UL1 -> [xss, [file10], yss, ulResultParameters, [file20], zss] ; UL0 -> [xss, ulAccessParameters, [file10], yss, [file20], zss] ; ~bbb -> [xss, ulAccessParameters, [file10], yss, ulResultParameters, [file20], zss] }) ""
 | otherwise = error "Sound.SoXBasics.soxOpG: At least one of the two given files has inappropriate file extension. Please, check the arguments. "
    where (file10, file20) = applyExts2 ul file1 file2

-- | The variant of the 'soxOpG' that is used if the second file is not used (or in the situation where some
-- other file is used, too, e. g. with the .prof extension). For the functions in the module, this corresponds
-- to the \"-n\" second file argument.
soxOpG1 :: ULencode -> [String] -> FilePath -> [String] -> [String] -> IO (ExitCode, String, String)
soxOpG1 ul xss file1 yss zss
 | (fst . getExts $ ul) `isSuffixOf` file1 =
    if ul < UL0 then readProcessWithExitCode (fromJust (showE "sox")) (filter (not . null) . mconcat $
       [xss, [file1], yss, ["-n"], zss]) ""
    else readProcessWithExitCode (fromJust (showE "sox")) (filter (not . null) . mconcat $
       [xss, ulAccessParameters, [file1], yss, ["-n"], zss]) ""
 | otherwise = error "Sound.SoXBasics.soxOpG1: A given file has inappropriate file extension. Please, check the arguments. "

-- | Function 'getMaxAG' returns a maximum amplitude of the sound in the file in the given lower and upper bounds represented as a tuple of 'Int' values.
getMaxAG :: ULencode -> FilePath -> (Int, Int) -> IO String
getMaxAG ul file (lowerbound, upperbound) = if isJust (showE "sox")
  then do
    (_, _, herr) <- soxOpG1 ul [] file [] ["trim", show lowerbound ++ "s", "=" ++ show upperbound ++ "s", "stat"]
    let zs = lines herr in return (let u = (words $ zs !! 3) !! 2 in if take 1 u == "-" then take 9 u else take 8 u)
  else do
    catchEnd ExecutableNotProperlyInstalled
    return []

-- | Function 'getMinAG' returns a minimum amplitude of the sound in the file in the given lower and upper bounds represented as a tuple of 'Int' values.
getMinAG :: ULencode -> FilePath -> (Int, Int) -> IO String
getMinAG ul file (lowerbound, upperbound) = if isJust (showE "sox")
  then do
    (_, _, herr1) <- soxOpG1 ul [] file [] ["trim", show lowerbound ++ "s", "=" ++ show upperbound ++ "s", "stat"]
    let zs = lines herr1 in return (let u = (words $ zs !! 4) !! 2 in if take 1 u == "-" then take 9 u else take 8 u)
  else do
    catchEnd ExecutableNotProperlyInstalled
    return []

-- | Function 'selMaxAbsG' returns a maximum by absolute value amplitude of the sound and allows by its second value in the tuple determine whether it is a maximum or minimum.
-- Bool 'True' corresponds to maximum value, 'False' - to minimum value.
selMaxAbsG :: ULencode -> FilePath -> (Int, Int) -> IO (String, Bool)
selMaxAbsG ul file (lowerbnd, upperbnd) = do
  tX <- getMaxAG ul file (lowerbnd, upperbnd)
  tN <- getMinAG ul file (lowerbnd, upperbnd)
  return (maxAbs (tX, tN))

-- | Function 'selMAG' returns a maximum or a minimum of the sound amplitude of the file depending on the @Bool@ value given.
-- Bool 'True' corresponds to maximum value, 'False' - to minimum value.
selMAG :: ULencode -> FilePath -> (Int, Int) -> Bool -> IO String
selMAG ul file (lowerbnd, upperbnd) x = if x then getMaxAG ul file (lowerbnd, upperbnd) else getMinAG ul file (lowerbnd, upperbnd)

-- | Function 'extremeSG' returns an approximate sample number of the extremum, which will be used further for fade effect.
extremeSG :: ULencode -> FilePath -> (Int, Int) -> Int -> IO (String, Bool) -> IO Int
extremeSG ul file (lowerbnd, upperbnd) eps x = if compare (upperbnd - lowerbnd) (eps + 33) == LT
  then return $ (upperbnd + lowerbnd) `quot` 2
  else do
    (ys, z) <- x
    let t = (lowerbnd + upperbnd) `quot` 2
    rs <- selMAG ul file (lowerbnd, t) z
    if (ys == rs)
         then extremeSG ul file (lowerbnd, t) eps x
         else extremeSG ul file (t, upperbnd) eps x

-- | Function 'alterVadBG' removes an approximate silence measured by the absolute value of the sound amplitude from the beginning of the file.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). The file must have maximum amplitude absolute value close to 1 before call to the 'alterVadBG'.
-- The second @Float@ parameter is used to exit the iteration cycle. The 'Int' parameter from the range [0..3] specifies a maximum amplitude, starting from
-- which the sound will not be trimmed.
alterVadBG :: ULencode -> FilePath -> Float -> Int -> Float -> IO ()
alterVadBG ul file lim noiseMax exit
 | compare lim exit /= GT = putStrLn $ "File " ++ file ++ " is ready for further processing."
 | otherwise =
  if isJust (showE "sox")
   then do
     lim1 <- durationAG ul file
     alterVadHelpG ul file lim1 lim noiseMax exit
   else catchEnd ExecutableNotProperlyInstalled

doubleCleanCheck :: FilePath -> FinalException -> IO ()
doubleCleanCheck file exception = do
  e0 <- doesFileExist file
  if e0 then removeFile file >> catchEnd exception else catchEnd exception

-- | Function 'alterVadHelpG' is used internally in the 'alterVadBG' and 'alterVadEG' functions.
alterVadHelpG :: ULencode -> FilePath -> Float -> Float -> Int -> Float -> IO ()
alterVadHelpG ul file lim1 lim noiseMax exit
 | compare lim1 lim == LT = alterVadBG ul file lim1 noiseMax exit
 | compare lim1 lim == EQ =
   let noiseM = (case noiseMax of
                 0 -> "0.01"
                 1 -> "0.02"
                 2 -> "0.04"
                 3 -> "0.08"
                 _ -> "0.04") in do
        (_, _, herr) <- soxOpG1 ul [] file [] ["trim", "0", showFFloat Nothing (lim1 / 2.0) $ show 0, "stat"]
        let zs = lines herr in let z = concatMap (dropWhile (not . isDigit)) . take 1 . drop 3 $ zs in if z < noiseM
          then do
            (code, _, _) <- soxOpG ul [] file10 [] file20 ["trim", showFFloat Nothing (lim1 / 2.0) $ show 0, "-0.000000"]
            if code == ExitSuccess then threadDelay 100000 >> opFileG ul file10 file20 exit noiseMax
            else doubleCleanCheck file20 MaybePartiallyTrimmed
          else alterVadBG ul file10 (lim1 / 4.0) noiseMax exit
 | otherwise =
  let noiseM = (case noiseMax of
                 0 -> "0.01"
                 1 -> "0.02"
                 2 -> "0.04"
                 3 -> "0.08"
                 _ -> "0.04") in do
        (_, _, herr) <- soxOpG1 ul [] file [] ["trim", "0", showFFloat Nothing (lim / 2.0) $ show 0, "stat"]
        let zs = lines herr in let z = concatMap (dropWhile (not . isDigit)) . take 1 . drop 3 $ zs in if z < noiseM
          then do
            (code, _, _) <- soxOpG ul [] file10 [] file20 ["trim", showFFloat Nothing (lim / 2.0) $ show 0, "-0.000000"]
            if code == ExitSuccess then threadDelay 100000 >> opFileG ul file10 file20 exit noiseMax
            else doubleCleanCheck file20 MaybePartiallyTrimmed
          else alterVadBG ul file10 (lim / 4.0) noiseMax exit
             where (file10, file20) = applyExts2 ul file ("7" ++ file)

-- | Function 'opFileG' is used internally in 'alterVadB' to check whether 'FilePath' exist and if so to do some processing to allow the 'alterVadB' function iterate further.
opFileG :: ULencode -> FilePath -> FilePath -> Float -> Int -> IO ()
opFileG ul file1 file2 exit noiseMax = do
  removeFile file1
  exist0 <- doesFileExist file1
  if exist0
    then opFileG ul file1 file2 exit noiseMax
    else do
      renameFile file2 file1
      lim2 <- durationAG ul file1
      alterVadBG ul file1 lim2 noiseMax exit

presenseCheck :: FilePath -> FinalException -> IO ()
presenseCheck file exception = do
  e2 <- doesFileExist file
  if e2 then return () else catchEnd exception

twoExceptions1File :: ExitCode -> FilePath -> FinalException -> FinalException -> IO ()
twoExceptions1File code file exc1 exc2 =
  if code /= ExitSuccess then doubleCleanCheck file exc1 else presenseCheck file exc2

-- | Function 'norm' applies a SoX normalization effect on the audio file.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
normG :: ULencode -> FilePath -> IO ()
normG ul file = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["norm"]
    twoExceptions1File code file20 (NotCreatedWithEffect "norm") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("8" ++ file)

-- | Function 'normLG' applies a SoX gain effect on the audio file with the maximum absolute dB value given by the 'Int' argument.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
normLG :: ULencode -> FilePath -> Int -> IO ()
normLG ul file level = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["gain", "-n", show level]
    twoExceptions1File code file20 (NotCreatedWithEffect "gain -n") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("9" ++ file)

-- | Function 'normLG' applies a SoX \"gain -b [db-Value]\" effect on the audio file with dB value given by the @Float@ argument.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
gainLG :: ULencode -> FilePath -> Float -> IO ()
gainLG ul file level = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["gain", "-b", showFFloat (Just 6) level $ show 0]
    twoExceptions1File code file20 (NotCreatedWithEffect "gain -b") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("9" ++ file)

-- | Function 'soxStatG' prints a SoX statistics for the audio file.
soxStatG :: ULencode -> FilePath -> IO ()
soxStatG ul file = if isJust (showE "sox")
  then do
    (_, _, herr) <- soxOpG1 ul [] file [] ["stat"]
    putStrLn herr
  else catchEnd ExecutableNotProperlyInstalled

secondFileClean :: FilePath -> FilePath -> FinalException -> IO ()
secondFileClean file1 file2 exception = do
  e1 <- doesFileExist file2
  if e1 then removeFile file2 else putStr ""
  removeFile file1
  catchEnd exception

twoIntermediateFs :: ExitCode -> FilePath ->  FilePath -> FilePath -> FinalException -> IO ()
twoIntermediateFs code file1 file2 file3 exception = do
  if code /= ExitSuccess
  then secondFileClean file1 file2 exception
  else do
    e2 <- doesFileExist file2
    if e2
      then do
        removeFile file1
        removeFile file3
        renameFile file2 file3
      else do
        removeFile file1
        catchEnd exception

-- | Function 'alterVadE' removes an approximate silence measured by the absolute value of the sound amplitude from the end of the file.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). The second @Float@ parameter is used to exit the iteration cycle. The 'Int' parameter
-- from the range [0..3] specifies a maximum amplitude, starting from which the sound will not be trimmed.
alterVadEG :: ULencode -> FilePath -> Float -> Int -> Float -> IO ()
alterVadEG ul file lim noiseMax exit
 | compare lim exit /= GT = putStrLn $ "File " ++ file ++ " is ready for further processing"
 | otherwise =
  if isJust (showE "sox")
   then do
     (code, _, _) <- soxOpG ul [] file10 [] file20 ["reverse"]
     if code /= ExitSuccess
      then doubleCleanCheck file20 (NotCreated file10)
      else do
        alterVadBG ul file20 lim noiseMax exit
        (code1, _, _) <- soxOpG ul [] file30 [] file40 ["reverse"]
        twoIntermediateFs code1 file20 file40 file10 (NotCreated file10)
   else catchEnd ExecutableNotProperlyInstalled
       where (file10, file20) = applyExts2 ul file ("6" ++ file)
             (file30, file40) = applyExts2 ul file20 ("76" ++ file10)

-- | Function 'upperBndG' returns a maximum number of samples for use in other functions.
upperBndG :: ULencode -> FilePath -> IO Int
upperBndG ul file = if isJust (showE "soxi")
  then do {
    (_, Just hout, _, _) <- createProcess (proc (fromJust (showE "soxi")) (if ul < UL0 then ["-s",file] else mconcat [["-s"],ulAccessParameters,[file]])){ std_out = CreatePipe } ;
    x0 <- hGetContents hout ;
    let z = read x0::Int in return z }
  else catchEnd ExecutableNotProperlyInstalled >> return (0::Int)

-- | Variant of the function 'extremeSG' with all the additional information included.
extremeSG1G :: ULencode -> FilePath -> IO Int
extremeSG1G ul file = do
  upp <- upperBndG ul file
  extremeSG ul file (0::Int, upp) (if upp `quot` 32 > 2 then upp `quot` 32 else 2::Int) (selMaxAbsG ul file (0::Int, upp))

-- | Function 'quarterSinFadeG' applies a fade effect by SoX to the audio file with \"q\" type.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
quarterSinFadeG :: ULencode -> FilePath -> IO ()
quarterSinFadeG ul file = if isJust (showE "sox")
  then do
    pos <- extremeSG1G ul file
    upp <- upperBndG ul file
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["fade", "q", show pos ++ "s", "=" ++ show upp ++ "s", show (upp - pos) ++ "s"]
    twoExceptions1File code file20 (NotCreatedWithEffect "fade q") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("4" ++ file)

-- | Function 'silenceBothG' adds some silence to both ends of the audio.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
silenceBothG :: ULencode -> FilePath -> Int -> Int -> IO ()
silenceBothG ul file beginning end = if isJust (showE "sox")
  then do
    _ <- soxOpG ul [] file10 [] file20 ["delay", show beginning ++ "s", "reverse"]
    _ <- soxOpG ul [] file20 [] file40 ["delay", show end ++ "s", "reverse"]
    removeFile file20
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("3" ++ file)
            (file30, file40) = applyExts2 ul file20 ("2" ++ file10)

-- | Function 'resampleAG' changes the sample rate for the recorded audio for further processing.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
resampleAG :: ULencode -> FilePath -> Int -> IO ()
resampleAG ul file frequency = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["rate", "-s", "-I", show frequency]
    twoExceptions1File code file20 (NotCreatedWithEffect "rate") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("3" ++ file)

-- | Function 'durationAG' returns a duration of the audio file in seconds.
durationAG :: ULencode -> FilePath -> IO Float
durationAG ul file = if isJust (showE "soxi")
  then do
    (_, Just hout, _, _) <- createProcess (proc (fromJust (showE "soxi")) (if ul < UL0 then ["-D",file] else mconcat [["-D"],ulAccessParameters,[file]])) { std_out = CreatePipe }
    x0 <- hGetContents hout
    let z = read x0::Float in return z
  else catchEnd ExecutableNotProperlyInstalled >> return 0.0

-- | Function 'noiseProfBG' creates with SoX a file containing a noise profile for the first 0.05 s of the audio file given.
noiseProfBG :: ULencode -> FilePath -> IO ()
noiseProfBG ul file = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG1 ul [] file [] ["trim", "0", "0.05", "noiseprof",file ++ ".b.prof"]
    twoExceptions1File code (file ++ ".b.prof") (NoiseProfileNotCreatedB file) (NoiseProfileNotCreatedB file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseProfEG' creates with SoX a file containing a noise profile for the last 0.05 s of the audio file given.
noiseProfEG :: ULencode -> FilePath -> IO ()
noiseProfEG ul file = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG1 ul [] file [] ["trim", "-0.05", "0.05", "noiseprof",file ++ ".e.prof"]
    twoExceptions1File code (file ++ ".e.prof") (NoiseProfileNotCreatedE file) (NoiseProfileNotCreatedE file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceBG' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfBG' function.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
noiseReduceBG :: ULencode -> FilePath -> IO ()
noiseReduceBG ul file = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["noisered", file10 ++ ".b.prof"]
    twoExceptions1File code file20 (NotCreatedWithEffect "noisered") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("_" ++ file)

-- | Function 'noiseReduceEG' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfEG' function.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
noiseReduceEG :: ULencode -> FilePath -> IO ()
noiseReduceEG ul file = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["noisered", file10 ++ ".e.prof"]
    twoExceptions1File code file20 (NotCreatedWithEffect "noisered") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("_." ++ file)

-- | Function 'noiseReduceBUG' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfBUG' function.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). The @Float@ parameter is a number between 0 and 1 showing the level of
-- reducing the noise (the greater number means that the function will reduce more intensively may be even aggressively so that for greater
-- numbers it can remove some sensitive and important sound data as a noise). Internally this parameter is passed unchanged to the \"sox\"
-- so that it uses it as an amount parameter for the \"noisered\" effect. Therefore, please, (as being stated in the SoX manual) experiment
-- with the amount to get suitable results.
noiseReduceBUG :: ULencode -> FilePath -> Float -> IO ()
noiseReduceBUG ul file amount = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["noisered", file10 ++ ".b.prof", showFFloat (Just 4) amount $ show 0]
    twoExceptions1File code file20 (NotCreatedWithEffect "noisered") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("_" ++ file)

-- | Function 'noiseReduceEUG' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfEGU' function.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). The @Float@ parameter is a number between 0 and 1 showing the level of
-- reducing the noise (the greater number means that the function will reduce more intensively may be even aggressively so that for greater
-- numbers it can remove some sensitive and important sound data as a noise). Internally this parameter is passed unchanged to the \"sox\"
-- so that it uses it as an amount parameter for the \"noisered\" effect. Therefore, please, (as being stated in the SoX manual) experiment
-- with the amount to get suitable results.
noiseReduceEUG :: ULencode -> FilePath -> Float -> IO ()
noiseReduceEUG ul file amount = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["noisered", file10 ++ ".e.prof", showFFloat (Just 4) amount $ show 0]
    twoExceptions1File code file20 (NotCreatedWithEffect "noisered") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("_." ++ file)

-- | Function 'volS' changes the given audio with the linear ratio for the amplitude so that the resulting amlitude is equal to the given @Float@ parameter.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from).
volSG :: ULencode -> FilePath -> Float -> IO ()
volSG ul file amplitude = if isJust (showE "sox")
  then do
    normG ul file
    e0 <- doesFileExist $ "8" ++ file
    if e0
      then do
        (code, _, _) <- soxOpG ul [] file10 [] file20 ["vol", showFFloat Nothing amplitude $ show 0, "amplitude"]
        if code /= ExitSuccess
          then secondFileClean file10 file20 (NotCreatedWithEffect "vol")
          else presenseCheck file20 (InitialFileNotChanged file10)
      else catchEnd (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul ("8" ++ file) ("8." ++ file)

-- | Function 'volS2G' changes the given audio (the first 'FilePath' parameter, which must be normalized e. g. by the 'norm' function before) with
-- the linear ratio for the amplitude so that the resulting amlitude is equal to the maximum by absolute value amplitude for the file given
-- by the second 'FilePath' parameter. The function must be used with the first 'FilePath' parameter containing no directories in its name
-- (that means the file of the first 'FilePath' parameter must be in the same directory where the function is called from).
volS2G :: ULencode -> FilePath -> FilePath -> IO ()
volS2G ul fileA fileB = if isJust (showE "sox")
  then do
    upp <- upperBndG ul fileB
    amplMax <- selMAG ul fileB (0, upp) True
    amplMin <- selMAG ul fileB (0, upp) False
    let ampl = read (fst . maxAbs $ (amplMax, amplMin))::Float
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["vol", showFFloat Nothing ampl $ show 0, "amplitude"]
    twoExceptions1File code file20 (NotCreatedWithEffect "vol") (InitialFileNotChanged fileA)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul fileA ("8." ++ drop 1 fileA)

-- | Function 'sincAG' uses a \"sinc\" effect with @-a 50 -I 0.07k-11k@ band-pass filter for the audio file given.
sincAG :: ULencode -> FilePath -> IO ()
sincAG ul file = if isJust (showE "sox")
  then do
    (code, _, _) <- soxOpG ul [] file10 [] file20 ["sinc", "-a", "50", "-I", "0.07k-11k"]
    twoExceptions1File code file20 (NotCreatedWithEffect "sinc") (InitialFileNotChanged file10)
  else catchEnd ExecutableNotProperlyInstalled
      where (file10, file20) = applyExts2 ul file ("4." ++ file)

-- | Function 'sampleAnG' analyzes the one sample of the 1-channel sound file (or k samples for the k-channel file) and returns a tuple pair of
-- the maximum and minimum amplitudes of the sound given as 'String's. For the 1-channel sound file they are the same.
-- The 'Integer' parameter is the number of the sample, starting from which SoX analyzes the sound. If it is less than number of the samples available,
-- then the function returns the value for the last one sample for the 1-channel file (or the last k samples for the k-channel sound file).
-- The file must not be in a RAW format for the function to work properly.
sampleAnG :: ULencode -> FilePath -> Integer -> IO (String, String)
sampleAnG ul file pos = if isJust (showE "sox") && isJust (showE "soxi")
  then onException (do
    (_, hout, _) <- readProcessWithExitCode (fromJust (showE "soxi")) (if ul < UL0 then ["-s", file] else mconcat [["-s"],ulAccessParameters,[file]]) ""
    let length0 = read hout::Integer
        f param = do
          (_, _, herr) <- soxOpG1 ul [] file [] ["trim", show param ++ "s", "1s", "stat"]
          let lns = map (last . words) . drop 3 . take 5 . lines $ herr in return (head lns, last lns)
    if compare length0 (fromIntegral pos) == GT
      then f pos
      else f (length0 - 1)) (catchEnd (NotEnoughData file))
  else catchEnd ExecutableNotProperlyInstalled >> return ("","")

---------------------------------------------------------------

-- | Function 'playA' plays the given file with SoX. For Windows it uses \"-t waveaudio -d\" options for SoX.
playA :: FilePath -> IO ()
playA file 
   | take 5 os == "mingw" = 
      if isJust (showE "sox") 
          then readProcessWithExitCode (fromJust (showE "sox")) [file, "-t", "waveaudio", "-d"] "" >> return ()
          else catchEnd ExecutableNotProperlyInstalled
   | otherwise = 
      if isJust (showE "play") 
          then readProcessWithExitCode (fromJust (showE "play")) [file] "" >> return ()
          else catchEnd ExecutableNotProperlyInstalled

--------------------------------------------------------------

getMaxA = getMaxAG W

getMinA = getMinAG W

selMaxAbs = selMaxAbsG W

selMA = selMAG W

extremeS = extremeSG W

extremeS1 = extremeSG1G W

soxStat = soxStatG W

upperBnd = upperBndG W

durationA = durationAG W

sampleAn = sampleAnG W

alterVadB = alterVadBG W

alterVadE = alterVadEG W

alterVadHelp = alterVadHelpG W

opFile = opFileG W

norm = normG W

normL = normLG W

gainL = gainLG W

quarterSinFade = quarterSinFadeG W

silenceBoth = silenceBothG W

resampleA = resampleAG W

noiseProfB = noiseProfBG W

noiseProfE = noiseProfEG W

noiseReduceB = noiseReduceBG W

noiseReduceE = noiseReduceEG W

noiseReduceBU = noiseReduceBUG W

noiseReduceEU = noiseReduceEUG W

sincA = sincAG W

volS = volSG W

volS2 = volS2G W

