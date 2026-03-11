-- |
-- Module      :  MMSyn7l
-- Copyright   :  (c) OleksandrZhabenko 2020-2022, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A program and a library to modify the amplitude of the sound representations for 
-- the Ukrainian language created by mmsyn7ukr package or somehow otherwise. Besides
-- it can be used to adjust volume for the sequential \"result*.wav\" files.
--

module MMSyn7l where

import qualified Data.List as L (sort,isPrefixOf,isSuffixOf)
import System.Directory
import Data.Char (toUpper, isDigit)
import qualified Sound.SoXBasics as SB
import qualified Sound.SoXBasics1 as SB1
import Data.Maybe (isJust)
import Control.Exception (onException)
import CaseBi.Arr (getBFstL')
import Sound.Control.Exception.FinalException
import System.Process
import System.Exit (ExitCode(ExitSuccess))
import EndOfExe2 (showE)
import Data.Maybe (fromJust)
import Numeric
import System.Info (os)

-- | Function 'changeVolume' is used to change the amplitude of the sound. 
-- For the proper proceeding you specify a @String@, which consists of 4 
-- digits (and it may be preceded by a symbol \"-\"). If the @String@ 
-- begins with the \"-\", then a sound amplitude decreases, otherwise 
-- the amplitude increases. The level of increase / decrease is 
-- determined by the magnitude of the absolute value of integer numbers. 
-- The greater is the number -- the greater is the amplitude change. 
-- The count begins with \"0000\" and ends with \"9999\" (the sign is not 
-- taken into consideration). If there is less than 4 digits in a @String@, 
-- then the @String@ is equivalent to that one with the appropriate number 
-- of zeroes preceding to fulfill to the 4 needed digits (for example, 
-- \"657\" is equivalent to \"0657\", \"-2\" is equivalent to \"-0002\" etc.) 
-- In such a case, for the @String@s without the 
-- initial sign \"-\" (the sound increases) an interval between the 
-- maximum by modulus value of the amlitude (which is represented by the 
-- parts of 1) and 1.0 is divided into 10 equal parts (starting a count 
-- from 0) and then that one of them is selected, which has a number 
-- determined by the first digit in the @String@ writing. Then (if specified 
-- further) the interval between this amplitude value and a value, 
-- which corresponds to the selection on the previous step the next 
-- first digit in the writing (for example, after \"4\" -- \"5\", 
-- after \"7\" -- \"8\" etc.), greater by 1 than the actually selected one, 
-- is also divided again into 10 equal parts and that one is selected, 
-- which corresponds to the number determined by the second digit in the 
-- String writing (again beginning with \"0\" and ending with \"9\") and so on 
-- until the 4th level. The greater exactness is not needed because our 
-- hearing ability hardly distinguish such a subtle sound changes. If 
-- the @String@ has as a first element the \'-\' @Char@ (the sound decreases), 
-- then everything is analogously the same, but an interval between the 
-- maximum by modulus amplitude value and 0.0 is divided into 10 equal parts 
-- and so on. 'Int' parameter is used to control the informational output
-- (to get it, specify 1). 
changeVolume ::  Int -> FilePath -> IO ()
changeVolume n file = do
  case (take 5 os == "mingw") of { True -> if isJust (showE "sox") then readProcessWithExitCode (fromJust (showE "sox"))
    [file, "-t", "waveaudio", "-d"] "" >> return () else catchEnd ExecutableNotProperlyInstalled ; ~rrr -> if isJust (showE "play") then readProcessWithExitCode (fromJust (showE "play")) [file] "" >> return () else catchEnd ExecutableNotProperlyInstalled } -- Is taken from the mmsyn7ukr package as playA.
  case n of
   1 -> do 
    let sound = getBFstL' "е" (zip ["A.wav", "B.wav", "C.wav", "D.wav", "E.wav", "F.wav", "G.wav", "H.wav", 
         "I.wav", "J.wav", "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav", 
          "S.wav", "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", "a.wav", "b.wav", "c.wav", 
            "d.wav", "e.wav", "f.wav"] $ ["а","б","в","г","д","дж","дз", "е","ж","з","и","й","к","л","м","н","о","п","р",
              "с","сь","т","у","ф","х","ц","ць","ч","ш","ь","і","ґ"]) file
    putStrLn $ "You can now change the volume for the played sound representation for the Ukrainian sound " ++ show (map toUpper sound)
    putStrLn ""
    putStr "Please, specify the change by passing a String of digits (with may be a preceding symbol \'-\'). "
    putStr ""
    putStr "For the proper proceeding you specify a String, which consists of 4 "
    putStr "digits (and it may be preceded by a symbol \"-\"). If the String "
    putStr "begins with the \"-\", then a sound amplitude decreases, otherwise "
    putStr "the amplitude increases. The level of increase / decrease is "
    putStr "determined by the magnitude of the absolute value of integer numbers. "
    putStr "The greater is the number -- the greater is the amplitude change. "
    putStr "The count begins with \"0000\" and ends with \"9999\" (the sign is not "
    putStr "taken into consideration). If there is less than 4 digits in a String, "
    putStr "then the String is equivalent to that one with the appropriate number "
    putStr "of zeroes preceding to fulfill to the 4 needed digits (for example, "
    putStr "\"657\" is equivalent to \"0657\", \"-2\" is equivalent to \"-0002\" etc.). "
    putStr "In such a case, for the Strings without the "
    putStr "initial sign \"-\" (the sound increases) an interval between the "
    putStr "maximum by modulus value of the amlitude (which is represented by the "
    putStr "parts of 1) and 1.0 is divided into 10 equal parts (starting a count "
    putStr "from 0) and then that one of them is selected, which has a number "
    putStr "determined by the first digit in the String writing. Then (if specified "
    putStr "further) the interval between this amplitude value and a value, "
    putStr "which corresponds to the selection on the previous step the next "
    putStr "first digit in the writing (for example, after \"4\" -- \"5\", "
    putStr "after \"7\" -- \"8\" etc.), greater by 1 than the actually selected one, "
    putStr "is also divided again into 10 equal parts and that one is selected, "
    putStr "which corresponds to the number determined by the second digit in the "
    putStr "String writing (again beginning with \"0\" and ending with \"9\") and so on "
    putStr "until the 4th level. The greater exactness is not needed because our "
    putStr "hearing ability hardly distinguish such a subtle sound changes. If "
    putStr "the String has as a first element the \'-\' Char (the sound decreases), "
    putStr "then everything is analogously the same, but an interval between the "
    putStr "maximum by modulus amplitude value and 0.0 is divided into 10 equal parts "
    putStrLn "and so on. "
    onException (specifyVol file) (do
      putStrLn ""
      putStrLn "Something went wrong for the sound representation, please, check the input value and repeat once more! "
      specifyVol file)
   _ -> onException (specifyVol file) (do
         putStrLn ""
         putStrLn "Something went wrong for the sound representation, please, check the input value and repeat once more! "
         specifyVol file)

-- | Function 'specifyVol' is used internally in the 'changeVolume' to get the @String@ and to apply the needed change. 
specifyVol :: FilePath -> IO ()
specifyVol file = do   
  change0 <- getLine
  upperbound <- SB.upperBnd file
  (originalStr, bool) <- SB.selMaxAbs file (0::Int, upperbound)
  if bool 
    then changeVol3 file (change0, originalStr)
    else changeVol4 file (change0, originalStr)

-- | Function 'changeVol2' is used internally in the 'specifyVol' in case of decreasing of the sound.
changeVol2 :: FilePath -> String -> Float -> IO ()
changeVol2 file xs ampl = do 
  let ys = take 4 . filter isDigit $ xs
      coefA = 0.0001 * fromIntegral (read ys::Int)
      ratio = 1.0 - coefA
  SB1.volS file (ratio * ampl)
  
-- | Function 'changeVol3' is used internally in the 'specifyVol' in case of working with the maximum amplitude.
changeVol3 :: FilePath -> (String, String) -> IO ()
changeVol3 file (change0, originalStr) = do
  let ampl = read originalStr::Float
  if ampl > 0.0 
    then do 
      let delta = 1.0 - ampl
          xs = filter (\x -> isDigit x || x == '-') change0
      if take 1 xs == "-" 
        then changeVol2 file xs ampl
        else do 
          let ys = take 4 . takeWhile (isDigit) $ xs
              coefA = 0.0001 * fromIntegral (read ys::Int)
              ratio = 1.0 + (delta / ampl) * coefA
          SB1.volS file (ratio * ampl)
    else catchEnd (StrangeAnswer "Sound.SoXBasics1" "volS")

-- | Function 'changeVol4' is used internally in the 'specifyVol' in case of working with the minimum amplitude.
changeVol4 :: FilePath -> (String, String) -> IO ()
changeVol4 file (change0, originalStr) = do
  let ampl = read originalStr::Float
  if ampl < 0.0
    then do 
      let delta = (-1.0) - ampl
          xs = filter (\x -> isDigit x || x == '-') change0
      if take 1 xs == "-" 
        then changeVol2 file xs ampl
        else do 
          let ys = take 4 . filter isDigit $ xs
              coefA = 0.0001 * fromIntegral (read ys::Int)
              ratio = 1.0 + (delta / ampl) * coefA
          SB1.volS file (ratio * ampl)
    else catchEnd (StrangeAnswer "Sound.SoXBasics1" "volS")

-- | Works with the \"result*.wav\" files in the current directory: it adjusts volume levels for the sequence of them starting from the 
-- first argument in a list and ending with the second one (if specified). If there is no first -- all such files are adjusted; if there is no second one -- 
-- the files are adjusted to the last one. Count starts at 0. 
adjustVolRes :: [String] -> IO ()
adjustVolRes args = do
  dir <- listDirectory "."
  let dirV0 = L.sort . filter (L.isPrefixOf "result") $ dir
      first0 = concat . take 1 $ args
      last0  = concat . take 1 . drop 1 $ args
      idxAllN = length dirV0 - 1
  onException (do { 
    let first1 = read first0::Int
        last1 = read last0::Int
        first2 = min (abs first1) (abs last1)
        last2 = max (abs first1) (abs last1)
        first
         | first2 > idxAllN = 0
         | otherwise = first2
        l =  if compare last2 idxAllN == GT then idxAllN - first + 1 else last2 - first + 1
     ; mapM_ (changeVolume 1) . take l . drop first $ dirV0
     ; putStrLn ""
     ; putStrLn "Now you have changed (or left unchanged) the amplitudes for the needed \"result*.wav\" sound files." }) (do
         error "Please, specify a right numbers for the first and last files to be adjusted starting count from 0.")

-----------------------------------------------------------------------------------------------

-- Taken from the DobutokO.Sound.IntermediateF module here so that they are more used this way.  

-- | Takes a filename to be applied a SoX chain of effects (or just one) as list of 'String' (the second argument). Produces the temporary
-- new file with the name ((name-of-the-file) ++ (\"effects.wav\"  OR \"effects.flac\") -- the type is preserved), which then is removed. 
--
-- The syntaxis is that every separate literal for SoX must be a new element in the list. Please, for more information, refer to SoX documentation.
-- Please, check by yourself whether you have enough permissions to read and write to the 'FilePath'-specified
-- file and to the containing it directory. The function is not intended to be used in otherwise cases.
soxE :: FilePath -> [String] -> IO ()
soxE file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "effects" ++ efw2 file] ++ arggs) ""
  case code of
    ExitSuccess -> renameFile (file ++ "effects" ++ efw2 file) file
    _ -> do
       removeFile $ file ++ "effects" ++ efw2 file
       putStrLn $ "MMSyn7l.soxE \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "

-- | Applies \"fade q\" effect to both ends of the supported by SoX sound file 'FilePath' so that concatenating them consequently after such application 
-- leads to no clipping. Otherwise, the clipping exists if not prevented by may be some other means. For more information, please, refer to the
-- SoX documentation.
fadeEnds :: FilePath -> IO ()
fadeEnds = fadeEndsMilN 10

-- | Applies \"fade q\" effect to both ends of the supported by SoX sound file 'FilePath' so that concatenating them consequently after such application 
-- leads to no clipping. Otherwise, the clipping exists if not prevented by may be some other means. The duration of the changes are in 5 times 
-- smaller than for 'fadeEnds' function and is equal to 0.002 sec. For more information, please, refer to the SoX documentation.
fadeEndsMil2 :: FilePath -> IO ()
fadeEndsMil2 = fadeEndsMilN 2

-- | Applies \"fade q\" effect to both ends of the supported by SoX sound file 'FilePath' so that concatenating them consequently after such application 
-- leads to no clipping. Otherwise, the clipping exists if not prevented by may be some other means. The duration of the changes are usually 
-- smaller than for 'fadeEnds' function and is equal to 0.001 \* n sec (where n is in range [1..10]). 
-- For more information, please, refer to the SoX documentation.
fadeEndsMilN :: Int -> FilePath -> IO ()
fadeEndsMilN n file = soxE file ["fade","q", showFFloat (Just 4) (if (n `rem` 11) /= 0 then 0.001 * fromIntegral (n `rem` 11) else 0.002) "","-0.0"]

-- | Applies \"fade\" effect (the type is specified by the 'Char' argument, for more information, please, refer to the SoX documentation) to the both ends 
-- of the sound with header (supported by SoX). The 'Float' arguments specify the percentages of the length of the sound that is faded-in and faded-out 
-- respectively. Otherwise, the function returns an error.
fadeEndsTMN :: Char -> Float -> Float -> FilePath -> IO ()
fadeEndsTMN c per1 per2 file 
 | compare per1 0 == GT && compare per2 0 == GT && compare (per1 + per2) 100 /= GT = do
    d0 <- SB.durationA file
    soxE file ["fade", getBFstL' "l" [('h',"h"),('p',"p"),('q',"q"),('t',"t")] c, showFFloat (Just 4) (d0 * per1 / 100.0) "","-0.0", 
      showFFloat (Just 4) (d0 * per2 / 100.0) ""]
 | otherwise = error "MMSyn7l.fadeEndsTMN: the percentages sum is out of the (0..100] range. "

-- | Variant of the 'fadeEndsTMN' with the both equal percentages specified by the 'Float' argument. It must be in the range (0..50]. Otherwise, the function 
-- returns error.
fadeEndsTMB :: Char -> Float -> FilePath -> IO ()
fadeEndsTMB c per 
 | compare per 0 == GT && compare per 50 /= GT = fadeEndsTMN c per per
 | otherwise = error "MMSyn7l.fadeEndsTMB: the percentage is out of the (0..50] range. "

------------------------------------------------------------------------------------------------------

-- New functions with usage of the moved here fadeEnds* and 'soxE' functions.

-- | Using a new function 'fadeEndsMil2' fades the ends of the .wav files in the current directory.
fadeWav002 :: IO ()
fadeWav002 = do
  dir0 <- fmap (filter (`L.isSuffixOf` ".wav")) . listDirectory $ "."
  mapM_ fadeEndsMil2 dir0

-- | Using a new function 'fadeEndsMilN' fades the ends of the .wav files in the current directory. The first argument is the 'Int' argument 
-- for the 'fadeEndsMilN' and is a number of milliseconds to be used for fading (usually from 1 to 10). 
fadeWavN :: Int -> IO ()
fadeWavN n = do
  dir0 <- fmap (filter (`L.isSuffixOf` ".wav")) . listDirectory $ "."
  mapM_ (fadeEndsMilN n) dir0        

-- | Converts WAV file to FLAC file using SoX (please, check before whether your installation supports FLAC files) using possible rate and bit depth
-- conversion accordingly to 'soxBasicParams' format. If the conversion is successful ('ExitCode' is 'ExitSuccess') then removes the primary file.
wavToFlac :: String -> FilePath -> IO ()
wavToFlac ys file = do
  let (ts,zs) = splitAt 2 . init $ ys
  (code,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file,getBFstL' "-r22050" (zip ["11","16", "17", "19", "32", "44", "48",
     "80", "96"] ["-r11025","-r16000","-r176400","-r192000","-r32000","-r44100","-r48000","-r8000","-r96000"]) ts, if zs == "2" then "-b24"
       else "-b16",take (length file - 3) file ++ "flac"] ""
  case code of
    ExitSuccess -> removeFile file
    _           -> do
      putStrLn $ "MMSyn7l.wavToFlac: " ++ herr
      exi <- doesFileExist $ take (length file - 3) file ++ "flac"
      if exi then removeFile (take (length file - 3) file ++ "flac") >> error ""
      else error ""

-- | Converts FLAC file to WAV file using SoX (please, check before whether your installation supports FLAC files) using possible rate and bit depth
-- conversion accordingly to 'soxBasicParams' format. If the conversion is successful ('ExitCode' is 'ExitSuccess') then removes the primary file.
flacToWav :: String -> FilePath -> IO ()
flacToWav ys file = do
  let (ts,zs) = splitAt 2 . init $ ys
  (code,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file,getBFstL' "-r22050" (zip ["11","16", "17", "19", "32", "44", "48",
     "80", "96"] ["-r11025","-r16000","-r176400","-r192000","-r32000","-r44100","-r48000","-r8000","-r96000"]) ts, if zs == "2" then "-b24"
       else "-b16",take (length file - 4) file ++ "wav"] ""
  case code of
    ExitSuccess -> removeFile file
    _           -> do
      putStrLn $ "MMSyn7l.flacToWav: " ++ herr
      exi <- doesFileExist $ take (length file - 4) file ++ "wav"
      if exi then removeFile (take (length file - 4) file ++ "wav") >> error ""
      else error ""      

w2f :: FilePath -> FilePath
w2f file = let (zs,ts) = splitAt (length file - 4) file in if ts == ".wav" then zs ++ ".flac" else error "You give not a WAV file! "
     
f2w :: FilePath -> FilePath
f2w file = let (zs,ts) = splitAt (length file - 5) file in if ts == ".flac" then zs ++ ".wav" else error "You give not a FLAC file! "

wOrf :: FilePath -> String
wOrf file = let us = drop (length file - 4) file in case us of { ".wav" -> "w" ; "flac" -> "f" ;
  ~bbb -> error "You give neither a WAV nor a FLAC file! " }

cfw2wf :: FilePath -> FilePath
cfw2wf file
 | wOrf file == "w" = w2f file
 | wOrf file == "f" = f2w file
 | otherwise = error "You give neither a WAV nor a FLAC file! "

efw2 :: FilePath -> String
efw2 file = let us = drop (length file - 4) file in case us of { ".wav" -> ".wav" ; "flac" -> ".flac" ;
  ~bbb -> error "You give neither a WAV nor a FLAC file! " }

efw2vv :: FilePath -> String
efw2vv file = let us = drop (length file - 4) file in case us of { ".wav" -> ".flac" ; "flac" -> ".wav" ;
  ~bbb -> error "You give neither a WAV nor a FLAC file! " }
