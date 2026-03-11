-- |
-- Module      :  SoXBasics.Arr
-- Copyright   :  (c) OleksandrZhabenko 2019-2022, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A program and a library that can be used as a simple 
-- basic interface to some SoX functionality or for producing 
-- the approximately Ukrainian speech with your own recorded 
-- voice (actually it produces the needed sound representations).
--


module SoXBasics.Arr (
  -- * Get Information
  maxAbs
  , getMaxA
  , getMinA
  , selMaxAbs
  , selMA
  , extremeS
  , extremeS1
  , soxStat
  , upperBnd
  , durationA
  , sampleAn
  -- * Produce sound
  -- ** Trimming the silence
  , alterVadB
  , alterVadE
  , alterVadHelp
  , opFile
  -- ** Amplitude modification
  , norm
  , normL
  , gainL
  , quarterSinFade
  -- ** Adding silence
  , silenceBoth
  -- ** Recording
  , recA
  , recB
  -- ** Changing sample rate
  , resampleA
  -- ** Working with noise
  , noiseProfB
  , noiseProfE
  , noiseReduceB
  , noiseReduceE
  , noiseReduceBU
  , noiseReduceEU
  -- ** Filtering
  , sincA
  -- ** Volume amplification
  , volS
  , volS2
  -- * Playing sound
  , playA
) where

import System.Directory
import Data.Maybe (isJust, fromJust)
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

-- | Function 'maxAbs' allows to choose a maximum by absolute value if the values are written as @String@. Bool @True@ corresponds to maximum value, @False@ - to minimum value
maxAbs :: (String, String) -> (String, Bool)
maxAbs (xs, ys) | null xs || null ys = ([], False)
                | head xs == '-' && head ys == '-' = if compare xs ys /= LT then (xs, False) else (ys, False)
                | head xs /= '-' && head ys /= '-' = if compare xs ys == GT then (xs, True) else (ys, True)
                | head xs == '-' && head ys /= '-' = if compare (tail xs) ys /= LT then (xs, False) else (ys, True)
                | otherwise = if compare xs (tail ys) == GT then (xs, True) else (ys, False)

-- | Function 'getMaxA' returns a maximum amplitude of the sound in the file in the given lower and upper bounds represented as a tuple of @Int@ values.
getMaxA :: FilePath -> (Int, Int) -> IO String
getMaxA file (lowerbound, upperbound) = if isJust (showE "sox") 
  then do
    (_, _, herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "trim", show lowerbound ++ "s", "=" ++ show upperbound ++ "s", "stat"] ""
    let zs = lines herr in return (let u = (words $ zs !! 3) !! 2 in if head u == '-' then take 9 u else take 8 u)
  else do
    catchEnd ExecutableNotProperlyInstalled
    return []

-- | Function 'getMinA' returns a minimum amplitude of the sound in the file in the given lower and upper bounds represented as a tuple of @Int@ values.  
getMinA :: FilePath -> (Int, Int) -> IO String
getMinA file (lowerbound, upperbound) = if isJust (showE "sox") 
  then do
    (_, _, herr1) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "trim", show lowerbound ++ "s", "=" ++ show upperbound ++ "s", "stat"] ""
    let zs = lines herr1 in return (let u = (words $ zs !! 4) !! 2 in if head u == '-' then take 9 u else take 8 u)
  else do
    catchEnd ExecutableNotProperlyInstalled
    return []

-- | Function 'selMaxAbs' returns a maximum by absolute value amplitude of the sound and allows by its second value in the tuple determine whether it is a maximum or minimum. 
-- Bool @True@ corresponds to maximum value, @False@ - to minimum value.
selMaxAbs :: FilePath -> (Int, Int) -> IO (String, Bool)
selMaxAbs file (lowerbnd, upperbnd) = do 
  tX <- getMaxA file (lowerbnd, upperbnd)
  tN <- getMinA file (lowerbnd, upperbnd)
  return (maxAbs (tX, tN))

-- | Function 'selMA' returns a maximum or a minimum of the sound amplitude of the file depending on the @Bool@ value given. 
-- Bool @True@ corresponds to maximum value, @False@ - to minimum value.
selMA :: FilePath -> (Int, Int) -> Bool -> IO String
selMA file (lowerbnd, upperbnd) x = if x then getMaxA file (lowerbnd, upperbnd) else getMinA file (lowerbnd, upperbnd)

-- | Function 'extremeS' returns an approximate sample number of the extremum, which will be used further for fade effect.
extremeS :: FilePath -> (Int, Int) -> Int -> IO (String, Bool) -> IO Int
extremeS file (lowerbnd, upperbnd) eps x = if compare (upperbnd - lowerbnd) (eps + 33) == LT 
  then return $ (upperbnd + lowerbnd) `quot` 2
  else do   
    (ys, z) <- x
    let t = (lowerbnd + upperbnd) `quot` 2 
    rs <- selMA file (lowerbnd, t) z
    if (ys == rs) 
         then extremeS file (lowerbnd, t) eps x
         else extremeS file (t, upperbnd) eps x

-- | Function 'alterVadB' removes an approximate silence measured by the absolute value of the sound amplitude from the beginning of the file. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). The file must have maximum amplitude absolute value close to 1 before call to the 'alterVadB'. 
-- The second @Float@ parameter is used to exit the iteration cycle. The @Int@ parameter from the range [0..3] specifies a maximum amplitude, starting from 
-- which the sound will not be trimmed.
alterVadB :: FilePath -> Float -> Int -> Float -> IO ()
alterVadB file lim noiseMax exit | compare lim exit /= GT = putStrLn $ "File " ++ file ++ " is ready for further processing."
                                 | otherwise = 
 if isJust (showE "sox")
  then do
         lim1 <- durationA file
         alterVadHelp file lim1 lim noiseMax exit  
  else catchEnd ExecutableNotProperlyInstalled
  
-- | Function 'alterVadHelp' is used internally in the 'alterVadB' and 'alterVadE' functions. 
alterVadHelp :: FilePath -> Float -> Float -> Int -> Float -> IO ()
alterVadHelp file lim1 lim noiseMax exit | compare lim1 lim == LT = alterVadB file lim1 noiseMax exit
                                         | compare lim1 lim == EQ = 
 let noiseM = (case noiseMax of 
                0 -> "0.01"
                1 -> "0.02"
                2 -> "0.04"
                3 -> "0.08"
                _ -> "0.04") in do 
       (_, _, herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "trim", "0", showFFloat Nothing (lim1 / 2.0) $ show 0, "stat"] ""
       let zs = lines herr in let z = concatMap (dropWhile (not . isDigit)) . take 1 . drop 3 $ zs in if z < noiseM
          then do 
            (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "7" ++ file, "trim", showFFloat Nothing (lim1 / 2.0) $ show 0, "-0.000000"] ""
            if code == ExitSuccess
              then do
                threadDelay 100000
                opFile file exit noiseMax
              else do
                e0 <- doesFileExist $ "7" ++ file
                if e0
                  then do
                    removeFile $ "7" ++ file
                    catchEnd MaybePartiallyTrimmed
                  else catchEnd MaybePartiallyTrimmed
          else alterVadB file (lim1 / 4.0) noiseMax exit  
                                         | otherwise = 
 let noiseM = (case noiseMax of 
                0 -> "0.01"
                1 -> "0.02"
                2 -> "0.04"
                3 -> "0.08"
                _ -> "0.04") in do 
       (_, _, herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "trim", "0", showFFloat Nothing (lim / 2.0) $ show 0, "stat"] ""
       let zs = lines herr in let z = concatMap (dropWhile (not . isDigit)) . take 1 . drop 3 $ zs in if z < noiseM
          then do 
            (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "7" ++ file, "trim", showFFloat Nothing (lim / 2.0) $ show 0, "-0.000000"] ""
            if code == ExitSuccess
              then do
                threadDelay 100000
                opFile file exit noiseMax
              else do
                e0 <- doesFileExist $ "7" ++ file
                if e0
                  then do
                    removeFile $ "7" ++ file
                    catchEnd MaybePartiallyTrimmed
                  else catchEnd MaybePartiallyTrimmed
          else alterVadB file (lim / 4.0) noiseMax exit

-- | Function 'opFile' is used internally in 'alterVadB' to check whether @FilePath@ exist and if so to do some processing to allow the 'alterVadB' function iterate further.
opFile :: FilePath -> Float -> Int -> IO ()
opFile file exit noiseMax = do
  removeFile file
  exist0 <- doesFileExist file
  if not exist0 
    then do 
      renameFile ("7" ++ file) file
      lim2 <- durationA file
      alterVadB file lim2 noiseMax exit
    else opFile file exit noiseMax

-- | Function 'norm' applies a SoX normalization effect on the audio file. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'normL' applies a SoX gain effect on the audio file with the maximum absolute dB value given by the @Int@ argument. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'normL' applies a SoX \"gain -b [db-Value]\" effect on the audio file with dB value given by the @Float@ argument. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'soxStat' prints a SoX statistics for the audio file.
soxStat :: FilePath -> IO ()
soxStat file = if isJust (showE "sox") 
  then do 
    (_, _, herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "stat"] ""
    putStrLn herr
  else catchEnd ExecutableNotProperlyInstalled
  
-- | Function 'alterVadE' removes an approximate silence measured by the absolute value of the sound amplitude from the end of the file. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). The second @Float@ parameter is used to exit the iteration cycle. The @Int@ parameter 
-- from the range [0..3] specifies a maximum amplitude, starting from which the sound will not be trimmed.
alterVadE :: FilePath -> Float -> Int -> Float -> IO ()
alterVadE file lim noiseMax exit | compare lim exit /= GT = putStrLn $ "File " ++ file ++ " is ready for further processing"
                                 | otherwise = 
 if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "6" ++ file, "reverse"] ""
    if code /= ExitSuccess
      then do
        e0 <- doesFileExist $ "6" ++ file
        if e0
          then do
            removeFile $ "6" ++ file
            catchEnd (NotCreated file)
          else do
            catchEnd (NotCreated file)
      else do
        alterVadB ("6" ++ file) lim noiseMax exit
        (code1, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) ["6" ++ file, "76" ++ file, "reverse"] ""
        if code1 /= ExitSuccess
          then do
            e1 <- doesFileExist $ "76" ++ file
            if e1
              then do
                removeFile $ "76" ++ file
                removeFile $ "6" ++ file
                catchEnd (NotCreated file)
              else do
                removeFile $ "6" ++ file
                catchEnd (NotCreated file)
          else do
            e2 <- doesFileExist $ "76" ++ file
            if e2
              then do
                removeFile $ "6" ++ file
                removeFile file
                renameFile ("76" ++ file) file
              else do
                removeFile $ "6" ++ file
                catchEnd (NotCreated file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'upperBnd' returns a maximum number of samples for use in other functions.
upperBnd :: FilePath -> IO Int
upperBnd file = if isJust (showE "soxi") 
  then do 
    (_, Just hout, _, _) <- createProcess (proc (fromJust (showE "soxi")) ["-s",file]){ std_out = CreatePipe }
    x0 <- hGetContents hout
    let z = read x0::Int in return z
  else catchEnd ExecutableNotProperlyInstalled >> return (0::Int)

-- | Variant of the function 'extremeS' with all the additional information included.
extremeS1 :: FilePath -> IO Int
extremeS1 file = do
  upp <- upperBnd file
  extremeS file (0::Int, upp) (if upp `quot` 32 > 2 then upp `quot` 32 else 2::Int) (selMaxAbs file (0::Int, upp))

-- | Function 'quarterSinFade' applies a fade effect by SoX to the audio file with \"q\" type. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
quarterSinFade :: FilePath -> IO ()
quarterSinFade file = if isJust (showE "sox") 
  then do
    pos <- extremeS1 file
    upp <- upperBnd file
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'silenceBoth' adds some silence to both ends of the audio. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
silenceBoth :: FilePath -> Int -> Int -> IO ()
silenceBoth file beginning end = if isJust (showE "sox") 
  then do
    _ <- readProcessWithExitCode (fromJust (showE "sox")) [file, "3" ++ file, "delay", show beginning ++ "s", "reverse"] ""
    _ <- readProcessWithExitCode (fromJust (showE "sox")) ["3" ++ file, "2" ++ file, "delay", show end ++ "s", "reverse"] ""
    removeFile $ "3" ++ file
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'recA' records audio file with the given name and duration in seconds. For Windows it uses a default audio device and \"-t waveaudio -d\" option to the SoX.
recA :: FilePath -> Float -> IO ()
recA file x | isJust (showE "sox") && take 5 os == "mingw" = do 
  (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) ["-t","waveaudio","-d","-b16", "-c1", "-esigned-integer", "-L", file, "trim", "0.5", showFFloat Nothing x $ show 0] ""
  if code /= ExitSuccess
    then do
      e0 <- doesFileExist file
      if e0
        then do
          removeFile file
          catchEnd (NotRecorded file)
        else catchEnd (NotRecorded file)
    else do
      e1 <- doesFileExist file
      if e1
        then return ()
        else catchEnd (NotRecorded file)
            | isJust (showE "rec") = do
  (code, _, _) <- readProcessWithExitCode (fromJust (showE "rec")) ["-b16", "-c1", "-esigned-integer", "-L", file, "trim", "0.5", showFFloat Nothing x $ show 0] ""
  if code /= ExitSuccess
    then do
      e0 <- doesFileExist file
      if e0
        then do
          removeFile file
          catchEnd (NotRecorded file)
        else catchEnd (NotRecorded file)
    else do
      e1 <- doesFileExist file
      if e1
        then return ()
        else catchEnd (NotRecorded file)
            | otherwise = catchEnd ExecutableNotProperlyInstalled

-- | Function 'recB' records audio file with the given name and duration in seconds. For Windows it uses a default audio device and \"-t waveaudio -d\" option 
-- to the SoX. Unlike 'recA', the duration of the pause in seconds (before the SoX executable actually starts to record sound data after
-- an initialization of the sound recording device) is controlled by the second @Float@ function argument. 
recB :: FilePath -> (Float, Float) -> IO ()
recB file (x, y) | isJust (showE "sox") && take 5 os == "mingw" = do 
  (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) ["-t","waveaudio","-d","-b16", "-c1", "-esigned-integer", "-L", file, "trim", showFFloat Nothing y $ show 0, showFFloat Nothing x $ show 0] ""
  if code /= ExitSuccess
    then do
      e0 <- doesFileExist file
      if e0
        then do
          removeFile file
          catchEnd (NotRecorded file)
        else catchEnd (NotRecorded file)
    else do
      e1 <- doesFileExist file
      if e1
        then return ()
        else catchEnd (NotRecorded file)
                 | isJust (showE "rec") = do
  (code, _, _) <- readProcessWithExitCode (fromJust (showE "rec")) ["-b16", "-c1", "-esigned-integer", "-L", file, "trim", showFFloat Nothing y $ show 0, showFFloat Nothing x $ show 0] ""
  if code /= ExitSuccess
    then do
      e0 <- doesFileExist file
      if e0
        then do
          removeFile file
          catchEnd (NotRecorded file)
        else catchEnd (NotRecorded file)
    else do
      e1 <- doesFileExist file
      if e1
        then return ()
        else catchEnd (NotRecorded file)
                 | otherwise = catchEnd ExecutableNotProperlyInstalled

-- | Function 'resampleA' changes the sample rate for the recorded audio for further processing. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'durationA' returns a duration of the audio file in seconds.
durationA :: FilePath -> IO Float
durationA file = if isJust (showE "soxi") 
  then do
    (_, Just hout, _, _) <- createProcess (proc (fromJust (showE "soxi")) ["-D",file]){ std_out = CreatePipe }
    x0 <- hGetContents hout
    let z = read x0::Float in return z
  else catchEnd ExecutableNotProperlyInstalled >> return 0.0

-- | Function 'playA' plays the given file with SoX. For Windows it uses \"-t waveaudio -d\" options for SoX.
playA :: FilePath -> IO ()
playA file | take 5 os == "mingw" = 
  if isJust (showE "sox") 
    then readProcessWithExitCode (fromJust (showE "sox")) [file, "-t", "waveaudio", "-d"] "" >> return ()
    else catchEnd ExecutableNotProperlyInstalled
           | otherwise = if isJust (showE "play") 
  then readProcessWithExitCode (fromJust (showE "play")) [file] "" >> return ()
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseProfB' creates with SoX a file containing a noise profile for the first 0.05 s of the audio file given.
noiseProfB :: FilePath -> IO ()
noiseProfB file = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "trim", "0", "0.05", "noiseprof",file ++ ".b.prof"] ""
    if code /= ExitSuccess
      then do
        e0 <- doesFileExist $ file ++ ".b.prof"
        if e0
          then do
            removeFile $ file ++ ".b.prof"
            catchEnd (NoiseProfileNotCreatedB file)
          else catchEnd (NoiseProfileNotCreatedB file)
      else do 
        e1 <- doesFileExist $ file ++ ".b.prof"
        if e1
          then return ()
          else catchEnd (NoiseProfileNotCreatedB file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseProfE' creates with SoX a file containing a noise profile for the last 0.05 s of the audio file given. 
noiseProfE :: FilePath -> IO ()
noiseProfE file = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "trim", "-0.05", "0.05", "noiseprof",file ++ ".e.prof"] ""
    if code /= ExitSuccess
      then do
        e0 <- doesFileExist $ file ++ ".e.prof"
        if e0
          then do
            removeFile $ file ++ ".e.prof"
            catchEnd (NoiseProfileNotCreatedE file)
          else catchEnd (NoiseProfileNotCreatedE file)
      else do 
        e1 <- doesFileExist $ file ++ ".e.prof"
        if e1
          then return ()
          else catchEnd (NoiseProfileNotCreatedE file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceB' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfB' function. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceE' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfE' function. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceBU' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfBU' function. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). The @Float@ parameter is a number between 0 and 1 showing the level of
-- reducing the noise (the greater number means that the function will reduce more intensively may be even aggressively so that for greater
-- numbers it can remove some sensitive and important sound data as a noise). Internally this parameter is passed unchanged to the \"sox\"
-- so that it uses it as an amount parameter for the \"noisered\" effect. Therefore, please, (as being stated in the SoX manual) experiment
-- with the amount to get suitable results. 
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'noiseReduceEU' reduces with SoX a noise in the file given with the corresponding noise profile created with 'noiseProfEU' function. 
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from). The @Float@ parameter is a number between 0 and 1 showing the level of
-- reducing the noise (the greater number means that the function will reduce more intensively may be even aggressively so that for greater
-- numbers it can remove some sensitive and important sound data as a noise). Internally this parameter is passed unchanged to the \"sox\"
-- so that it uses it as an amount parameter for the \"noisered\" effect. Therefore, please, (as being stated in the SoX manual) experiment
-- with the amount to get suitable results. 
noiseReduceEU :: FilePath -> Float -> IO ()
noiseReduceEU file amount = if isJust (showE "sox") 
  then do
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "_." ++ file, "noisered", file ++ ".e.prof", showFFloat (Just 4) amount $ show 0] ""
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'volS' changes the given audio with the linear ratio for the amplitude so that the resulting amlitude is equal to the given @Float@ parameter.
-- The function must be used with the @FilePath@ parameter containing no directories in its name (that means the file of the @FilePath@ parameter must be 
-- in the same directory where the function is called from).
volS :: FilePath -> Float -> IO ()
volS file amplitude = if isJust (showE "sox") 
  then do
    norm file
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
              then return ()
              else do 
                removeFile $ "8" ++ file
                catchEnd (InitialFileNotChanged file)
      else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'volS2' changes the given audio (the first @FilePath@ parameter, which must be normalized e. g. by the 'norm' function before) with 
-- the linear ratio for the amplitude so that the resulting amlitude is equal to the maximum by absolute value amplitude for the file given 
-- by the second @FilePath@ parameter. The function must be used with the first @FilePath@ parameter containing no directories in its name 
-- (that means the file of the first @FilePath@ parameter must be in the same directory where the function is called from).
volS2 :: FilePath -> FilePath -> IO ()
volS2 fileA fileB = if isJust (showE "sox") 
  then do
    upp <- upperBnd fileB
    amplMax <- selMA fileB (0, upp) True
    amplMin <- selMA fileB (0, upp) False
    let ampl = read (fst . maxAbs $ (amplMax, amplMin))::Float
    (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) [fileA, "8." ++ tail fileA, "vol", showFFloat Nothing ampl $ show 0, "amplitude"] ""
    if code /= ExitSuccess 
      then do
        e1 <- doesFileExist $ "8." ++ tail fileA
        if e1
          then do
            removeFile $ "8." ++ tail fileA
            catchEnd (NotCreatedWithEffect "vol")
          else catchEnd (NotCreatedWithEffect "vol")
      else do 
        file8e <- doesFileExist $ "8." ++ tail fileA
        if file8e 
          then return ()
          else catchEnd (InitialFileNotChanged fileA)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'sincA' uses a \"sinc\" effect with @-a 50 -I 0.07k-11k@ band-pass filter for the audio file given.
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
          then return ()
          else catchEnd (InitialFileNotChanged file)
  else catchEnd ExecutableNotProperlyInstalled

-- | Function 'sampleAn' analyzes the one samle of the 1-channel sound file (or k samples for the k-channel file) and returns a tuple pair of 
-- the maximum and minimum amplitudes of the sound given as @String@s. For the 1-channel sound file they are the same. 
-- The @Integer@ parameter is the number of the sample, starting from which SoX analyzes the sound. If it is less than number of the samples available, 
-- then the function returns the value for the last one sample for the 1-channel file (or the last k samples for the k-channel sound file). 
-- The file must not be in a RAW format for the function to work properly.
sampleAn :: FilePath -> Integer -> IO (String, String)
sampleAn file pos = if isJust (showE "sox") && isJust (showE "soxi")
  then onException (do
    (_, hout, _) <- readProcessWithExitCode (fromJust (showE "soxi")) ["-s", file] ""
    let length0 = read hout::Integer
        f param = do 
          (_, _, herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file, "-n", "trim", show param ++ "s", "1s", "stat"] ""
          let lns = map (last . words) . drop 3 . take 5 . lines $ herr in return (head lns, last lns)
    if compare length0 (fromIntegral pos) == GT 
      then f pos
      else f (length0 - 1)) (catchEnd (NotEnoughData file))
  else catchEnd ExecutableNotProperlyInstalled >> return ("","")
