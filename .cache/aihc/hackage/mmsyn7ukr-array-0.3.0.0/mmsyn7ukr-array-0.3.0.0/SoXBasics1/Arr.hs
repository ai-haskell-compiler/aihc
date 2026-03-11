-- |
-- Module      :  SoXBasics1.Arr
-- Copyright   :  (c) OleksandrZhabenko 2020-2022, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A program and a library that can be used as a simple 
-- basic interface to some SoX functionality or for producing 
-- the approximately Ukrainian speech with your own recorded 
-- voice (actually it produces the needed sound representations). 
-- This module differs from a SoXBasics that the resulting files
-- in it have possibly just the same name as the input ones. The functions
-- try to replace the initial file with the processed one.
-- 


module SoXBasics1.Arr (
  -- * Produce sound
  -- ** Amplitude modification
  norm
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
import qualified SoXBasics.Arr as SB (extremeS1,upperBnd,selMA,maxAbs,norm)
import Sound.Control.Exception.FinalException

-- | Function 'norm' applies a SoX normalization effect on the audio file. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
norm :: FilePath -> IO ()
norm file = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "8" ++ file, "norm"] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "8" ++ file
        if e1
          then do
            removeFile $ "8" ++ file
            catchEnd (NotCreatedWithEffect "norm")
          else catchEnd (NotCreatedWithEffect "norm")
      else do 
        e2 <- doesFileExist $ "8" ++ file
        if e2 
          then do
            removeFile file
            renameFile ("8" ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'normL' applies a SoX gain effect on the audio file with the maximum absolute dB value given by the @Int@ argument. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
normL :: FilePath -> Int -> IO ()
normL file level = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "9" ++ file, "gain", "-n", show level] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "9" ++ file
        if e1
          then do
            removeFile $ "9" ++ file
            catchEnd (NotCreatedWithEffect "gain -n")
          else catchEnd (NotCreatedWithEffect "gain -n")
      else do 
        e2 <- doesFileExist $ "9" ++ file
        if e2 
          then do
            removeFile file
            renameFile ("9" ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'normL' applies a SoX \"gain -b [db-Value]\" effect on the audio file with dB value given by the @Float@ argument. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
gainL :: FilePath -> Float -> IO ()
gainL file level = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "9" ++ file, "gain", "-b", showFFloat (Just 6) level $ show 0] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "9" ++ file
        if e1
          then do
            removeFile $ "9" ++ file
            catchEnd (NotCreatedWithEffect "gain -b")
          else catchEnd (NotCreatedWithEffect "gain -b")
      else do 
        e2 <- doesFileExist $ "9" ++ file
        if e2 
          then do
            removeFile file
            renameFile ("9" ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'quarterSinFade' applies a fade effect by SoX to the audio file with \"q\" type. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
quarterSinFade :: FilePath -> IO ()
quarterSinFade file = if isJust (showE "sox") 
  then do
    pos <- SB.extremeS1 file
    upp <- SB.upperBnd file
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "4" ++ file, "fade", "q", show pos ++ "s", "=" ++ show upp ++ "s", show (upp - pos) ++ "s"] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "4" ++ file
        if e1
          then do
            removeFile $ "4" ++ file
            catchEnd (NotCreatedWithEffect "fade q")
          else catchEnd (NotCreatedWithEffect "fade q")
      else do 
        e2 <- doesFileExist $ "4" ++ file
        if e2 
          then do
            removeFile file
            renameFile ("4" ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'silenceBoth' adds some silence to both ends of the audio. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
silenceBoth :: FilePath -> Int -> Int -> IO ()
silenceBoth file beginning end = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "3" ++ file, "delay", show beginning ++ "s", "reverse"] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "3" ++ file
        if e1
          then do
            removeFile $ "3" ++ file
            catchEnd (NotCreatedWithEffects "delay reverse")
          else catchEnd (NotCreatedWithEffects "delay reverse")
      else do 
        e2 <- doesFileExist $ "3" ++ file
        if e2 
          then do
            (code1, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) ["3" ++ file, "2" ++ file, "delay", show end ++ "s", "reverse"] ""
            if code1 /= ExitSuccess
              then do
                e2 <- doesFileExist $ "2" ++ file
                if e2
                  then do
                    removeFile $ "3" ++ file
                    removeFile $ "2" ++ file
                    catchEnd (NotCreated file)
                  else do
                    removeFile $ "3" ++ file
                    catchEnd (NotCreated file)
              else do
                e3 <- doesFileExist $ "2" ++ file
                if e3
                  then do
                    removeFile $ "3" ++ file
                    removeFile file
                    renameFile ("2" ++ file) file
                  else do
                    removeFile $ "3" ++ file
                    catchEnd (NotCreated file)
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'resampleA' changes the sample rate for the recorded audio for further processing. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
resampleA :: FilePath -> Int -> IO ()
resampleA file frequency = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "3" ++ file, "rate", "-s", "-I", show frequency] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "3" ++ file
        if e1
          then do
            removeFile $ "3" ++ file
            catchEnd (NotCreatedWithEffect "rate")
          else catchEnd (NotCreatedWithEffect "rate")
      else do 
        e2 <- doesFileExist $ "3" ++ file
        if e2 
          then do
            removeFile file
            renameFile ("3" ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceB' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfB' function. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
noiseReduceB :: FilePath -> IO ()
noiseReduceB file = if isJust (showE "sox")
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "_" ++ file, "noisered", file ++ ".b.prof"] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "_" ++ file
        if e1
          then do
            removeFile $ "_" ++ file
            catchEnd (NotCreatedWithEffect "noisered")
          else catchEnd (NotCreatedWithEffect "noisered")
      else do 
        e2 <- doesFileExist $ "_" ++ file
        if e2 
          then do
            removeFile file
            renameFile ("_" ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceE' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfE' function. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
noiseReduceE :: FilePath -> IO ()
noiseReduceE file = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "_." ++ file, "noisered", file ++ ".e.prof"] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "_." ++ file
        if e1
          then do
            removeFile $ "_." ++ file
            catchEnd (NotCreatedWithEffect "noisered")
          else catchEnd (NotCreatedWithEffect "noisered")
      else do 
        e2 <- doesFileExist $ "_." ++ file
        if e2 
          then do
            removeFile file
            renameFile ("_." ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceBU' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfBU' function. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
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
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "_" ++ file, "noisered", file ++ ".b.prof", showFFloat (Just 4) amount $ show 0] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "_" ++ file
        if e1
          then do
            removeFile $ "_" ++ file
            catchEnd (NotCreatedWithEffect "noisered")
          else catchEnd (NotCreatedWithEffect "noisered")
      else do 
        e2 <- doesFileExist $ "_" ++ file
        if e2 
          then do
            removeFile file
            renameFile ("_" ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceEU' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfE' function. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
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
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "_." ++ file, "noisered", file ++ ".e.prof",  showFFloat (Just 4) amount $ show 0] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "_." ++ file
        if e1
          then do
            removeFile $ "_." ++ file
            catchEnd (NotCreatedWithEffect "noisered")
          else catchEnd (NotCreatedWithEffect "noisered")
      else do 
        e2 <- doesFileExist $ "_." ++ file
        if e2 
          then do
            removeFile file
            renameFile ("_." ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'volS' changes the given audio with the linear ratio for the amplitude so that the resulting amlitude is equal to the given @Float@ parameter.
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
volS :: FilePath -> Float -> IO ()
volS file amplitude = if isJust (showE "sox") 
  then do
    SB.norm file
    e0 <- doesFileExist $ "8" ++ file
    if e0
      then do
        (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) ["8" ++ file, "8." ++ file, "vol", showFFloat Nothing amplitude $ show 0, "amplitude"] ""
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

-- | Function 'volS2' changes the given audio (the first @FilePath@ parameter, which must be normalized e. g. by the 'norm' function before) with 
-- the linear ratio for the amplitude so that the resulting amlitude is equal to the maximum by absolute value amplitude for the file given 
-- by the second @FilePath@ parameter. The function must be used with the first @FilePath@ parameter containing no directories in its name 
-- (that means the file of the first @FilePath@ parameter must be in the same directory where the function is called from). While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
volS2 :: FilePath -> FilePath -> IO ()
volS2 fileA fileB = if isJust (showE "sox") 
  then do
    upp <- SB.upperBnd fileB
    amplMax <- SB.selMA fileB (0, upp) True
    amplMin <- SB.selMA fileB (0, upp) False
    let ampl = read (fst . SB.maxAbs $ (amplMax, amplMin))::Float
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [fileA, "8." ++ drop 1 fileA, "vol", showFFloat Nothing ampl $ show 0, "amplitude"] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "8." ++ drop 1 fileA
        if e1
          then do
            removeFile $ "8." ++ drop 1 fileA
            catchEnd (NotCreatedWithEffect "vol")
          else catchEnd (NotCreatedWithEffect "vol")
      else do 
        file8e <- doesFileExist $ "8." ++ drop 1 fileA
        if file8e 
          then do
            removeFile fileA
            renameFile ("8." ++ drop 1 fileA) fileA
          else catchEnd (InitialFileNotChanged fileA)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'sincA' uses a \"sinc\" effect with @-a 50 -I 0.07k-11k@ band-pass filter for the audio file given. While being
-- executed the function tries to replace the initial file with the resulting processed one and to clean the temporary files. If it is not
-- successful the function exits with exception of the type 'FinalException' and leaves the initial file without modification.
sincA :: FilePath -> IO ()
sincA file = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "4." ++ file, "sinc", "-a", "50", "-I", "0.07k-11k"] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "4." ++ file
        if e1
          then do
            removeFile $ "4." ++ file
            catchEnd (NotCreatedWithEffect "sinc")
          else catchEnd (NotCreatedWithEffect "sinc")
      else do 
        e0 <- doesFileExist $ "4." ++ file
        if e0 
          then do
            removeFile file
            renameFile ("4." ++ file) file
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

