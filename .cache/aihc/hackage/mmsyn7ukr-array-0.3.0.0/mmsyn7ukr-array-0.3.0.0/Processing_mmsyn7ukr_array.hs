-- |
-- Module      :  Processing_mmsyn7ukr
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


module Processing_mmsyn7ukr_array (
 -- * Producing sound
  produceSound2
  , produceSound3
  , produceSound4
  , beginProcessing
  , controlNoiseReduction
  -- * Additional functions
  , tempS
  , showCoef
  , tempeRa
  , recommendSharp
  -- * Cleaning
  , cleanTemp
  , cleanTempN
) where

import Control.Concurrent (threadDelay)
import Data.Typeable
import Numeric
import System.Directory
import Control.Exception (onException)
import EndOfExe2 (showE)
import Data.Maybe (fromJust)
import Data.Char
import qualified Data.List as L
import qualified Control.Monad as CM
import System.Process
import System.IO
import System.Info (os)
import System.Environment (getProgName)
import System.Exit
import Sound.SoXBasics
import CaseBi.Arr (getBFst')
import GHC.Arr
import Parser.ReplaceP (replaceP, replaceP4)
import Sound.Control.Exception.FinalException
import SoXBasics.Arr (recA,recB)

-- | Function 'produceSound3' is used internally in the 'produceSound2' function.
produceSound3 :: (String, String) -> (FilePath, FilePath) -> String -> (Int, Float) -> Float -> IO ()
produceSound3 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0
 | actsctrl == "-1" = prodSnd3H (actsctrl, noiseLim) (file, file1) soundUkr
 | take 1 actsctrl == "0" = prodSnd3H (actsctrl, noiseLim) (file, file1) soundUkr
 | take 1 actsctrl == "1" = do
     prodSnd3H2 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0 >>= \lim1 -> 
       if lim1 <= 0.0 then return () else resampleA "8_x.wav" (22050::Int) >> produceSound4 (file, file1) "38_x.wav"
 | take 1 actsctrl == "2" = do
    prodSnd3H2 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0 >>= \lim1 -> 
     if lim1 <= 0.0 then return () else sincA "8_x.wav" >> resampleA "4.8_x.wav" (22050::Int) >> produceSound4 (file, file1) "34.8_x.wav"
 | otherwise = do
    prodSnd3H2 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0 >>= \lim1 -> 
     if lim1 <= 0.0 then return () else sincA "8_x.wav" >> resampleA "4.8_x.wav" (22050::Int) >> quarterSinFade "34.8_x.wav" >> 
       produceSound4 (file, file1) "434.8_x.wav"

prodSnd3H :: (String, String) -> (FilePath, FilePath) -> String -> IO ()
prodSnd3H (actsctrl, noiseLim) (file, file1) soundUkr = do 
    lim1 <- durationA "8_x.wav"
    if lim1 <= 0.0
      then beginProcessing (file, file1) soundUkr (actsctrl, noiseLim)
      else do 
        resampleA "8_x.wav" (22050::Int)
        produceSound4 (file, file1)  "38_x.wav"

prodSnd3H2 :: (String, String) -> (FilePath, FilePath) -> String -> (Int, Float) ->  Float -> IO Float
prodSnd3H2 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0 = do
    alterVadB "8_x.wav" lim0 noiseMax (duration0*0.03)
    lim1 <- durationA "8_x.wav"
    if lim1 <= 0.0
      then beginProcessing (file, file1) soundUkr (actsctrl, noiseLim)
      else alterVadE "8_x.wav" lim1 noiseMax (duration0*0.03)
    return lim1

-- | Function 'produceSound4' is used internally in the 'produceSound3' function for amplification 
-- up/down to the maximum level of the first @FilePath@ parameter in the tuple. The second one gives 
-- a name of the resulting file and the third @FilePath@ parameter of the function is the @FilePath@ for 
-- the input file.
produceSound4 :: (FilePath, FilePath) -> FilePath -> IO ()
produceSound4 (file, file1) fileB = do 
  norm fileB
  volS2 ("8" ++ fileB) file
  renameFile ("8." ++ fileB) file1

-- | Function 'showCoef' is used to represent the duration of the sound file.
showCoef :: String -> String
showCoef xs | '.' `elem` xs = 
  let (ts, us) = break (== '.') xs in let ws = showFFloat (Just 6) ((fromIntegral (read (take 6 . drop 1 $ us)::Int) + 1.0) / 1000000.0) "" in 
    ts ++ drop 1 ws
            | otherwise = xs
      
-- | Function 'beginProcessing' is used to catch the variant where the sound is fully cut by the SoX because the sound was created in inappropriate time.
-- It returns the process to the beginning of the sound recording. For the meaning of the tuple of @Sring@ parameters, refer to 
-- 'produceSound' documentation. The first @FilePath@ in the tuple of @FilePath@ parameters is a name of the sound file in @mmsyn6ukr-array@ package. The second one is the 
-- name of the resulting file to be produced in the current directory.
beginProcessing :: (FilePath, FilePath) -> String -> (String, String) -> IO ()
beginProcessing (file, file1) soundUkr (actsctrl, noiseLim) = do {
  cleanTemp
; putStr "The needed files were NOT created, because the sound was not at the moment of recording! The process will be restarted "
; putStrLn "for the sound. Please, produce a sound during the first 3 seconds (after 0.5 second delay) or specify greater ratio!"
; putStrLn $ "Listen to the \"" ++ soundUkr ++ "\" sound and note first of all its duration. "
; playA file
; putStrLn "    *****"
; putStrLn ""
; putStrLn "The sound duration is: "
; produceSound2 (file, file1) (actsctrl, noiseLim) soundUkr}

-- | Function 'produceSound2' is used internally in the 'produceSound' function.
produceSound2 :: (FilePath, FilePath) -> (String, String) -> String -> IO ()
produceSound2 (file, file1) (actsctrl, noiseLim) soundUkr = do {
; duration0 <- durationA file
; putStrLn $ showCoef (showFFloat (Just 6) duration0 "")
; putStrLn ""
; putStrLn "It means that to produce more than 3 seconds of recording, you must specify at least "
; putStrLn $ "   " ++ show (3.0/duration0) ++ " as a next step ratio being prompt "
; putStrLn "   OR "
; putStrLn $ "   " ++ show (1.0/duration0) ++ " per one second but not less than the previous number."
; putStrLn $ "For example for 10 seconds record, please, specify " ++ show (10.0/duration0) ++ " as a next step ratio."
; putStrLn "    *****"
; putStrLn ""
; (_, Just hout, _, _) <- createProcess (proc (fromJust . showE $ "soxi") ["-D", file]) { std_out = CreatePipe }
; x3 <- hGetContents hout
; recommendSharp soundUkr
; (longerK0,pause0,sharp) <- tempS soundUkr noiseLim
; let longerK = (read x3::Float)*longerK0
; putStrLn $ "Please, wait for " ++ show pause0 ++ " seconds and pronounce the sound representation for the "
; putStrLn ""
; putStrLn $ "                                   \"" ++ (if soundUkr /= "ь" then map toUpper soundUkr else soundUkr) ++ "\""
; putStrLn ""
; putStrLn " sound or whatever you would like to be substituted instead (be sensible, please)! "
; if sharp || (compare longerK 3.0 == GT)
    then recB "x.wav" (longerK, pause0)
    else recB "x.wav" (3.0, pause0)
; putStrLn "The file is recorded and now will be automatically processed. You will be notificated with the text message in the terminal about the creation of the needed file. Please, wait a little. "
; controlNoiseReduction actsctrl
; norm "_x.wav"
; lim0 <- durationA "8_x.wav" 
; if null noiseLim
    then putStrLn ""
    else if last noiseLim == 's'
           then putStr ""
           else putStrLn ""
; let noiseMax = case (take 1 noiseLim) of
                   "0" -> 0::Int
                   "1" -> 1::Int
                   "2" -> 2::Int
                   "3" -> 3::Int
                   _ -> 2::Int
; produceSound3 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0
; cleanTemp }

-- | Function 'controlNoiseReduction' is used in the 'produceSound2' and 'beginProcessing' functions to reduce the noise with the created by the
-- 'tempeRa' noise profile. If you specified something else than \"-1\" as a first command line argument, then the program will reduce the noise
-- using the created noise profile. 
-- If the first character is one of the following, then the program will do the following actions besides. After the first character
-- (without any spaces) you can specify the level of noise reduction by 2 next digits. They are treated by the program as a fractional part
-- of the number \"0.\" ++ \"...\" so that the last number is passed to the SoX as an amount parameter in the \"noisered\" effect
-- (the greater number gives more aggressive noise reduction with the default one equal to 0.5. For more information, please, refer to the SoX documentation. 
controlNoiseReduction :: String -> IO ()
controlNoiseReduction actsctrl = if actsctrl /= "-1"
    then do
      s2 <- onException (do
              let s1 = take 1 actsctrl
                  sr0 = filter isDigit . drop 1 $ actsctrl
              if null sr0
                then return 0.5
                else let sr = "0." ++ sr0
                         s  = read sr::Float in if s > 0.0 then return s else return 0.01) (return 0.5)
      (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) ["x.wav", "_x.wav", "noisered", "nx0.wav.b.prof", showFFloat (Just 4) s2 $ show 0] ""
      if code /= ExitSuccess 
        then do
          e1 <- doesFileExist "_x.wav"
          if e1
            then do
              removeFile "_x.wav"
              catchEnd (NotCreatedWithEffect "noisered")
            else catchEnd (NotCreatedWithEffect "noisered")
        else do 
          e2 <- doesFileExist "_x.wav"
          if e2 
            then return ()
            else catchEnd (InitialFileNotChanged "x.wav")
    else renameFile "x.wav" "_x.wav"

-- | Function to get the @(Float, Float, Bool)@ value. The first @Float@ value shows in how many times you expect that your sound representation
-- will be longer than the one provided by the @mmsyn6ukr-array@ package. The second one specifies a duration of the pause before SoX actually starts to
-- record the needed sound data (in seconds). The @Bool@ value specifies whether the program uses a \'sharp\' mode meaning that
-- it does not check whether the resulting duration of the recording is at least 3 seconds long, so you can specify shorter durations.
-- The @String@ arguments are the Ukrainian sound representation name and the second command line argument for the program respectively.
tempS :: String -> String -> IO (Float, Float, Bool)
tempS soundUkr noiseLim = onException (do
    if null noiseLim
      then putStrLn ""
      else if last noiseLim == 's'
             then putStr ""
             else putStrLn ""
    putStrLn "In how many times do you think your sound representing " 
    putStrLn ""
    putStrLn $ "                     \"" ++ (if soundUkr /= "ь" then map toUpper soundUkr else soundUkr) ++ "\"" 
    putStrLn ""
    putStrLn "will sound longer than the recently played one? Specify your input as a Float value without \'e\' notation (with the preceding asterisk sign for the \'sharp\' mode). "
    putStrLn ""
    longivityZ <- getLine
    let sharp0 = take 1 longivityZ
        sharp1 = drop 1 . dropWhile (/= '#') $ longivityZ
        sharp2 = if null sharp1 then 0.5 else let zzz0 = read (filter (\z -> isDigit z || z == '.') sharp1)::Float in if zzz0 /= 0.0 then zzz0 else 0.5
    case sharp0 of
      "*" -> let long = read (takeWhile (/= '#') . drop 1 $ longivityZ)::Float in return (long,sharp2,True)
      _   -> let long = read (takeWhile (/= '#') longivityZ)::Float in return (long,sharp2,False)) (do 
               putStrLn "Please, specify again the valid values!"
               tempS soundUkr noiseLim)

-- | Function 'cleanTemp' removes all the intermediate temporary files in the directory where it is called from.
cleanTemp :: IO ()
cleanTemp = do
  filenames <- getDirectoryContents =<< getCurrentDirectory
  mapM_ removeFile . filter (\x -> head x `elem` (['2'..'9'] ++ "_" ++ "x")) $ filenames

-- | Function 'cleanTempN' removes all the intermediate temporary files produced during a noise profile creation in the directory where it is called from.
cleanTempN :: IO ()
cleanTempN = do
  filenames <- getDirectoryContents =<< getCurrentDirectory
  mapM_ removeFile . filter (\x -> head x == 'n') $ filenames

-- | Function 'tempeRa' is used to create a noise profile for all the recorded sounds. The function is used internally in the @mmsyn7ukr@
-- program. While running if you provide a 5 seconds silence as needed, the program @mmsyn7ukr@ will
-- reduce the noise in your recordings. This will create a cleaner sound. If you would like not to reduce the noise at all, then, please,
-- specify \"-1\" as the first command line argument for the program @mmsyn7ukr@.
tempeRa :: Int -> IO ()
tempeRa n = do {
    putStrLn "Now, please, be in a silence for 5 seconds so that the program can create a noise profile to remove the noise from the recording. "
    ; putStr "Otherwise, the program can remove from the recorded sound data some important parts as a noise. "
    ; if n == 1 then putStrLn "If you would like not to reduce the noise at all, then, please, specify as the first command line argument \"-1\". "
      else putStr ""
    ; CM.zipWithM_ (\i x -> recA ("nx" ++ show i ++ ".wav") 0.07 >> threadDelay (50000 * x)) [1..5] $ [2,3,2,2,3]
    ; [upperB1,upperB2,upperB3,upperB4,upperB5] <- mapM (\c -> upperBnd ("nx" ++ (c:".wav"))) "12345"
    ; v0 <- CM.zipWithM (\i upp -> fmap (\kt -> abs (read (fst kt)::Float))  . selMaxAbs ("nx" ++ show i ++ ".wav") $ (0,upp)) [1..5] $ [upperB1,upperB2,upperB3,upperB4,upperB5]
    ; let minn = minimum v0 in renameFile ("nx" ++ (show ((fromJust . L.findIndex (<= minn) $ v0) + 1) ++ ".wav")) "nx0.wav"
    ; noiseProfB "nx0.wav" >> putStrLn "" >> threadDelay 400000 >> putStrLn "The noise sound profile is now created. The program can proceed further." }

-- | Function 'recommendSharp' is used to print an advice about the speech transformation for the Ukrainian sounds that you can pronounce
-- properly continually and so it can be better to use for their producing a \'sharp\' mode.
recommendSharp :: String -> IO ()
recommendSharp soundUkr = do
  let k0 = getBFst' (False, listArray (0,16) . zip ["\1072","\1077","\1078","\1079","\1080","\1083","\1084","\1085","\1086","\1088","\1089","\1089\1100","\1091","\1092","\1093","\1096","\1110"] $ replicate 17 True) soundUkr
  if k0
    then do
      putStr $ "In case of speech transformation: for the sound representation for the Ukrainian \"" ++ soundUkr
      putStrLn "\" it is recommended to use a \'sharp\' mode. So, try to specify \'*\' as a first symbol and maybe pronounce the corresponding Ukrainian sound continually. "
    else do
      putStr $ "In case of speech transformation: for the sound representation for the Ukrainian \"" ++ soundUkr
      putStrLn "\" it is recommended to use a common mode (the usual one). So, try not to specify \'*\' as a first symbol. "


