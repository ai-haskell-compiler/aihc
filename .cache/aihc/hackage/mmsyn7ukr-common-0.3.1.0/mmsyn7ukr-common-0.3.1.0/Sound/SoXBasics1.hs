-- |
-- Module      :  Sound.SoXBasics1
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A program and a library that can be used as a simple basic interface to some SoX functionality.
-- This module differs from the "Sound.SoXBasics" that the resulting files
-- in it have possibly just the same name as the input ones. The functions
-- try to replace the initial file with the processed one. There is no possibility using these
-- functions to change the file extension. If you use this module and "Sound.SoXBasics" functionalities together,
-- please, use qualified import to avoid misusage.


module Sound.SoXBasics1 (
  -- * Produce sound
  -- ** General processment functions
  moveSnd2Fst
  , getULFromExt
  , twoExceptions2Files
  , threeFiles1Exception
  -- ** Amplitude modification
  , norm
  , normL
  , gainL
  , quarterSinFade
  -- ** Adding silence
  , silenceBoth
  -- ** Changing sample rate
  , resampleA
  -- ** Working with noise
  , noiseReduceB
  , noiseReduceE
  , noiseReduceBU
  , noiseReduceEU
  -- ** Filtering
  , sincA
  -- ** Volume amplification
  , volS
  , volS2
) where

import System.Directory
import Data.Maybe (isJust, fromJust)
import Numeric
import System.Process
import EndOfExe2
import System.Exit
import qualified Sound.SoXBasics as SB (ULencode(..), SoundFileExts(..), soxOpG, soxOpG1,
  ulAccessParameters, ulResultParameters, doubleCleanCheck, presenseCheck, secondFileClean,
    twoIntermediateFs, twoExceptions1File, applyExts2, beforeExtStr, extremeS1,upperBndG,selMAG,maxAbs,normG)
import Sound.Control.Exception.FinalException

moveSnd2Fst :: FilePath -> FilePath -> FinalException -> IO ()
moveSnd2Fst file1 file2 exception = do
  e2 <- doesFileExist file2
  if e2
    then do
      removeFile file1
      renameFile file2 file1
    else catchEnd exception

getULFromExt :: FilePath -> SB.ULencode
getULFromExt file =
 case end of
  ".wav" -> SB.W
  (z:".ul") -> SB.UL
  _  -> error "Sound.SoXBasics1.getULFromExt: The file has neither .wav, nor .ul extension."
  where l = length file - 4
        (begin,end) = splitAt l file

-- | Function 'norm' applies a SoX normalization effect on the audio file.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
norm :: FilePath -> IO ()
norm file = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("8" ++ file) ["norm"]
    if code /= ExitSuccess
      then SB.doubleCleanCheck ("8" ++ file) (NotCreatedWithEffect "norm")
      else moveSnd2Fst ("8" ++ file) file (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

twoExceptions2Files :: ExitCode -> FilePath -> FilePath -> FinalException -> FinalException -> IO ()
twoExceptions2Files code file1 file2 exc1 exc2 =
  if code /= ExitSuccess then SB.doubleCleanCheck file2 exc1 else moveSnd2Fst file1 file2 exc2

-- | Function 'normL' applies a SoX gain effect on the audio file with the maximum absolute dB value given by the 'Int' argument.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
normL :: FilePath -> Int -> IO ()
normL file level = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("9" ++ file) ["gain", "-n", show level]
    twoExceptions2Files code file ("9" ++ file) (NotCreatedWithEffect "gain -n") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'normL' applies a SoX \"gain -b [db-Value]\" effect on the audio file with dB value given by the @Float@ argument.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
gainL :: FilePath -> Float -> IO ()
gainL file level = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("9" ++ file) ["gain", "-b", showFFloat (Just 6) level $ show 0]
    twoExceptions2Files code file ("9" ++ file) (NotCreatedWithEffect "gain -b") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'quarterSinFade' applies a fade effect by SoX to the audio file with \"q\" type.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
quarterSinFade :: FilePath -> IO ()
quarterSinFade file = if isJust (showE "sox")
  then do
    pos <- SB.extremeS1 file
    upp <- SB.upperBndG (getULFromExt file) file
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("4" ++ file) ["fade", "q", show pos ++ "s", "=" ++ show upp ++ "s", show (upp - pos) ++ "s"]
    twoExceptions2Files code file ("4" ++ file) (NotCreatedWithEffect "fade q") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

threeFiles1Exception :: ExitCode -> FilePath -> FilePath -> FilePath -> FinalException -> IO ()
threeFiles1Exception code file1 file2 file3 exception
  | code /= ExitSuccess = do
      e2 <- doesFileExist file3
      if e2 then removeFile file3 else putStr ""
      removeFile file2
      catchEnd exception
  | otherwise = do
      e3 <- doesFileExist file3
      removeFile file2
      if e3
        then do
          removeFile file1
          renameFile file3 file1
        else catchEnd exception

-- | Function 'silenceBoth' adds some silence to both ends of the audio.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
silenceBoth :: FilePath -> Int -> Int -> IO ()
silenceBoth file beginning end = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("3" ++ file) ["delay", show beginning ++ "s", "reverse"]
    if code /= ExitSuccess
      then SB.doubleCleanCheck ("3" ++ file) (NotCreatedWithEffects "delay reverse")
      else do
        e2 <- doesFileExist $ "3" ++ file
        if e2
          then do
            (code1, _, _) <- SB.soxOpG (getULFromExt file) [] ("3" ++ file) [] ("2" ++ file) ["delay", show end ++ "s", "reverse"]
            threeFiles1Exception code1 file ("3" ++ file) ("2" ++ file) (NotCreated file)
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'resampleA' changes the sample rate for the recorded audio for further processing.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
resampleA :: FilePath -> Int -> IO ()
resampleA file frequency = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("3" ++ file) ["rate", "-s", "-I", show frequency]
    twoExceptions2Files code file ("3" ++ file) (NotCreatedWithEffect "rate") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceB' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfB' function.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
noiseReduceB :: FilePath -> IO ()
noiseReduceB file = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("_" ++ file) ["noisered", file ++ ".b.prof"]
    twoExceptions2Files code file ("_" ++ file) (NotCreatedWithEffect "noisered") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceE' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfE' function.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
noiseReduceE :: FilePath -> IO ()
noiseReduceE file = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("_." ++ file) ["noisered", file ++ ".e.prof"]
    twoExceptions2Files code file ("_." ++ file) (NotCreatedWithEffect "noisered") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceBU' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfBU' function.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). The @Float@ parameter is a number between 0 and 1 showing the level of
-- reducing the noise (the greater number means that the function will reduce more intensively may be even aggressively so that for greater
-- numbers it can remove some sensitive and important sound data as a noise). Internally this parameter is passed unchanged to the \"sox\"
-- so that it uses it as an amount parameter for the \"noisered\" effect. Therefore, please, (as being stated in the SoX manual) experiment
-- with the amount to get suitable results. While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
noiseReduceBU :: FilePath -> Float -> IO ()
noiseReduceBU file amount = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("_" ++ file) ["noisered", file ++ ".b.prof", showFFloat (Just 4) amount $ show 0]
    twoExceptions2Files code file ("_" ++ file) (NotCreatedWithEffect "noisered") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceEU' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfE' function.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). The @Float@ parameter is a number between 0 and 1 showing the level of
-- reducing the noise (the greater number means that the function will reduce more intensively may be even aggressively so that for greater
-- numbers it can remove some sensitive and important sound data as a noise). Internally this parameter is passed unchanged to the \"sox\"
-- so that it uses it as an amount parameter for the \"noisered\" effect. Therefore, please, (as being stated in the SoX manual) experiment
-- with the amount to get suitable results. While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
noiseReduceEU :: FilePath -> Float -> IO ()
noiseReduceEU file amount = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("_." ++ file) ["noisered", file ++ ".e.prof",  showFFloat (Just 4) amount $ show 0]
    twoExceptions2Files code file ("_." ++ file) (NotCreatedWithEffect "noisered") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'volS' changes the given audio with the linear ratio for the amplitude so that the resulting amlitude is equal to the given @Float@ parameter.
-- The function must be used with the 'FilePath' parameter containing no directories in its name (that means the file of the 'FilePath' parameter must be
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
volS :: FilePath -> Float -> IO ()
volS file amplitude = if isJust (showE "sox")
  then do
    SB.normG (getULFromExt file) file
    e0 <- doesFileExist $ "8" ++ file
    if e0
      then do
        (code, _, _) <- SB.soxOpG (getULFromExt file) [] ("8" ++ file) [] ("8." ++ file) ["vol", showFFloat Nothing amplitude $ show 0, "amplitude"]
        if code /= ExitSuccess
          then do
            e1 <- doesFileExist $ "8." ++ file
            if e1
              then do
                removeFile $ "8." ++ file
                removeFile $ "8" ++ file
                catchEnd (NotCreatedWithEffect "vol")
              else do
                removeFile $ "8" ++ file
                catchEnd (NotCreatedWithEffect "vol")
          else do
            e2 <- doesFileExist $ "8." ++ file
            if e2
              then do
                removeFile file
                removeFile $ "8" ++ file
                renameFile ("8." ++ file) file
              else do
                removeFile $ "8" ++ file
                catchEnd (InitialFileNotChanged file)
      else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'volS2' changes the given audio (the first 'FilePath' parameter, which must be normalized e. g. by the 'norm' function before) with
-- the linear ratio for the amplitude so that the resulting amlitude is equal to the maximum by absolute value amplitude for the file given
-- by the second 'FilePath' parameter. The function must be used with the first 'FilePath' parameter containing no directories in its name
-- (that means the file of the first 'FilePath' parameter must be in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
volS2 :: FilePath -> FilePath -> IO ()
volS2 fileA fileB = if isJust (showE "sox")
  then do
    upp <- SB.upperBndG (getULFromExt fileB) fileB
    amplMax <- SB.selMAG (getULFromExt fileB) fileB (0, upp) True
    amplMin <- SB.selMAG (getULFromExt fileB) fileB (0, upp) False
    let ampl = read (fst . SB.maxAbs $ (amplMax, amplMin))::Float
    (code, _, _) <- SB.soxOpG (getULFromExt fileA) [] fileA [] ("8." ++ drop 1 fileA) ["vol", showFFloat Nothing ampl $ show 0, "amplitude"]
    twoExceptions2Files code fileA ("8." ++ drop 1 fileA) (NotCreatedWithEffect "vol") (InitialFileNotChanged fileA)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'sincA' uses a \"sinc\" effect with @-a 50 -I 0.07k-11k@ band-pass filter for the audio file given. While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
sincA :: FilePath -> IO ()
sincA file = if isJust (showE "sox")
  then do
    (code, _, _) <- SB.soxOpG (getULFromExt file) [] file [] ("4." ++ file) ["sinc", "-a", "50", "-I", "0.07k-11k"]
    twoExceptions2Files code file ("4." ++ file) (NotCreatedWithEffect "sinc") (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

