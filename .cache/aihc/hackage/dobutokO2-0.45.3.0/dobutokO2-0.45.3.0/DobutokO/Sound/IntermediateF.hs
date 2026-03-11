-- |
-- Module      :  DobutokO.Sound.IntermediateF
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Måintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.


{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.IntermediateF (
  -- * Basic functions to work with intermediate files \"result\*wav\"
  getFileRSizes
  , getFileRSizesS
  , getFileRSizesS2
  , getFileRTuples
  , listVDirectory
  , isHighQ
  , shouldBeReplaced
  , indexesFromMrk
  -- * Functions to edit the melody by editing the intermediate files \"result\*\"
  , playAndMark
  , playAMrk
  , pAnR1
  , pAnR2
  , pAnR_
  -- ** 2G generalized variants
  , playAMrk2G
  , pAnR12G
  , pAnR22G
  , pAnR_2G
  -- * Additional functions
  -- ** Get information
  , infoFromV
  , internalConv
  , ixFromRes
  , ixInterv
  , ixInterv2G
  , thisOne
  -- ** Process and Edit
  , playSeqAR
  , playSeqARV
  , playSeqARV2
  , playCollect1Dec
  , playCollectDec
  , replaceWithHQs
  , isOddAsElem
  , maxLinV
  , minLinV
  , doubleLtoV
  , filterToBnds
  -- * 2G generalized functions
  , getFileRSizes2G
  , getFileRSizesS2G
  , getFileRTuples2G
  , listVDirectory2G
  -- * 3G generalized function
  , listVDirectory3G
  -- ** Process and Edit
  , playSeqAR2G
  , playSeqARV2G
  , playSeqARV22G
  , playCollectDec2G
  , replaceWithHQs2G
  -- * SoX effects application
  , soxBasicParams
  -- ** With \"reverb\" as the first
  -- *** No file type changes
  , reverbE
  , reverbWE
  , reverb1E
  , reverbW1E
  -- *** File type changes
  , reverbE2C
  , reverbWE2C
  , reverb1E2C
  , reverb1WE2C
  -- ** Generalized
  -- *** No file type changes
  , soxE1
  , getSilenceF
  , fadeAllE
  , fadeAllEMilN
  -- *** File type changes
  , soxE2C
  , soxE12C
  -- ** Playing and recording
  , recE
  , rec1E
  , playE
  -- * 2G auxiliary functions
  , f2w
  , w2f
  , cfw2wf
  , efw2
  , efw2vv
  , wOrf
  , wavToFlac
  , flacToWav
  -- * Special SoX effects
  , soxREw1
  , soxRE1
  , soxREA1
) where

import GHC.Arr
import Numeric (showFFloat)
import CaseBi.Arr (getBFst')
import Control.Monad (void)
import Control.Concurrent (myThreadId,forkIO,threadDelay,killThread)
import qualified Data.List as L (sort)
import Control.Exception (onException)
import Sound.Control.Exception.FinalException (FinalException (NotRecorded,ExecutableNotProperlyInstalled),catchEnd)
import Data.List (isPrefixOf,isSuffixOf,(\\),maximum,minimum)
import qualified Data.Vector as V 
import System.Directory
import Sound.SoXBasics (playA, durationA)
import MMSyn7l
import EndOfExe2 (showE)
import System.Process (readProcessWithExitCode)
import Data.Maybe (fromJust,isJust)
import System.Exit (ExitCode (ExitSuccess))
import System.Info (os)

-- | Gets sizes of the \"result\*.wav\" files in the current directory. 
getFileRSizes :: IO (V.Vector Integer)
getFileRSizes = getFileRSizes2G "221w"

-- | Generalized variant of the 'getFileRSizes' with a possibility to get sizes either of FLAC or of WAV files. For more information, please, refer to
-- 'soxBasicParams'.
getFileRSizes2G :: String -> IO (V.Vector Integer)
getFileRSizes2G ys = do
  dirN <- listDirectory "."
  let dirN1 = V.fromList . L.sort . filter (\s -> isPrefixOf "result" s && isSuffixOf (if drop 3 ys == "f" then ".flac" else ".wav") s) $ dirN
  V.mapM getFileSize dirN1  

-- | Similar to 'getFileRSizes', but sizes are 'Int', not 'Integer'. For most cases it is more memory efficient.
getFileRSizesS :: IO (V.Vector Int)
getFileRSizesS = getFileRSizesS2G "221w"

-- | Generalized variant of the 'getFileRSizesS' with a possibility to get sizes either of FLAC or of WAV files. For more information, please, refer to
-- 'soxBasicParams'.
getFileRSizesS2G :: String -> IO (V.Vector Int)
getFileRSizesS2G ys = do
  dirN0 <- listDirectory "."
  let dirN2 = V.fromList . L.sort . filter (\s -> isPrefixOf "result" s && isSuffixOf (if drop 3 ys == "f" then ".flac" else ".wav") s) $ dirN0
  sizes1 <- V.mapM getFileSize dirN2
  return . V.map fromIntegral $ sizes1

-- | Variant of 'getFileRSizes' function.
getFileRSizesS2 :: IO (V.Vector Int)
getFileRSizesS2 = getFileRSizes >>= \s -> return . V.map fromIntegral $ s

-- | Gets 'V.Vector' of tuples of the pairs of \"result\*.wav\" files and their respective sizes.
getFileRTuples :: IO (V.Vector (FilePath,Integer))
getFileRTuples = getFileRTuples2G "221w"

-- | Generalized variant of the 'getFileRTuples' with a possibility to get sizes either of FLAC or of WAV files. For more information, please, refer to
-- 'soxBasicParams'.
getFileRTuples2G :: String -> IO (V.Vector (FilePath,Integer))
getFileRTuples2G ys = do
  dirN <- listDirectory "."
  let dirN0 = L.sort . filter (\s -> isPrefixOf "result" s && isSuffixOf (if drop 3 ys == "f" then ".flac" else ".wav") s) $ dirN
  sizes0 <- mapM getFileSize dirN0
  let tpls = V.fromList . zip dirN0 $ sizes0
  return tpls  

-- | Gets 'V.Vector' of the filenames for \"result\*.wav\" files in the current directory.
listVDirectory :: IO (V.Vector FilePath)
listVDirectory = listVDirectory2G "221w"

-- | Generalized variant of the 'listVDirectory' with a possibility to get 'FilePath' for either FLAC or WAV files. For more information, please, refer to
-- 'soxBasicParams'.
listVDirectory2G :: String -> IO (V.Vector FilePath)
listVDirectory2G ys = do
  dir0N <- listDirectory "."
  let diNN = V.fromList . L.sort . filter (\s -> isPrefixOf "result" s && isSuffixOf (if drop 3 ys == "f" then ".flac" else ".wav") s) $ dir0N
  return diNN  

-- | Generalized variant of the 'listVDirectory2G' with a possibility to get 'FilePath' for not only \"result\*\" files, but to specify their 
-- beginning with the second 'String' argument. For example:
-- 
-- >  listVDirectory3G ys "result" == listVDirectory2G ys
-- 
listVDirectory3G :: String -> String -> IO (V.Vector FilePath)
listVDirectory3G ys zs = do
  dir0N <- listDirectory "."
  let diNN = V.fromList . L.sort . filter (\s -> isPrefixOf zs s && isSuffixOf (if drop 3 ys == "f" then ".flac" else ".wav") s) $ dir0N
  return diNN   

-- | During function evaluation you can listen to the sound files and mark them with \"1\" and \"0\". The first one means that the sound is considered
-- of higher quality and is intended to be used as a replacement for the worse sounds marked by \"0\". The function returns a 'V.Vector' of specially formatted
-- 'String' that represents only those files that are connected with the replacement procedure.
playAndMark :: V.Vector FilePath -> IO (V.Vector String)
playAndMark vec
  | V.null vec = return V.empty
  | otherwise = V.imapM (\i xs -> do
      duration <- durationA $ V.unsafeIndex vec i
      putStrLn "Listen to the next sound, please. Please, do not enter anything while sound plays. "
      forkIO $ do
        myThread <- myThreadId
        playA xs
        killThread myThread
      threadDelay (read (show $ truncate (duration * 1000000))::Int)
      putStr "How do you mark the file that has just been played now -- if of high quality, print \"1\", if of low quality, print \"0\", "
      putStrLn "if it is just accepted, press \'Enter\'. "  
      mark0 <- getLine
      putStrLn "-----------------------------------------------------------------------------------------"
      let mark = take 1 mark0
      case mark of
        "1" -> return $ show i ++ "*" ++ xs
        "0" -> return $ show i ++ "**" ++ xs
        _   -> return []) vec >>= V.filterM (return . not . null) 

-- | Function 'playAndMark' applied to all the \"result\*.wav\" files in the current directory.
playAMrk :: IO (V.Vector String)
playAMrk = playAMrk2G "221w"

-- | Generalized variant of the 'playAMrk' with a possibility to play and mark either FLAC or WAV files. For more information, please, refer to
-- 'soxBasicParams'.
playAMrk2G :: String -> IO (V.Vector String)
playAMrk2G ys = listVDirectory2G ys >>= playAndMark

-- | Function-predicate to check whether a file corresponding to its 'String' argument is needed to be replaced while processing.
shouldBeReplaced :: String -> Bool
shouldBeReplaced (x:y:xs)
  | x == '*' && y == '*' = True
  | otherwise = shouldBeReplaced (y:xs)
shouldBeReplaced _ = False

-- | Function-predicate to check whether a file corresponding to its 'String' argument is considered as one of higher quality and therefore can be used
-- to replace the not so suitable ones while processing.
isHighQ :: String -> Bool
isHighQ xs = (length . filter (== '*') $ xs) == 1

-- | Gets an index of the 'V.Vector' element corresponding to the 'String' generated by 'playAndMark' function.
indexesFromMrk :: String -> Int
indexesFromMrk xs = read (takeWhile (/= '*') xs)::Int

-- | Used to obtain parameters for processment.
internalConv :: ([String],[String]) -> (V.Vector Int, V.Vector String)
internalConv (xss,yss) = (V.fromList . map indexesFromMrk $ xss,V.fromList . map (dropWhile (== '*')) $ yss)

-- | Axiliary function to get a 'String' of consequent digits in the name of the \"result\*.wav\" file.
ixFromRes :: String -> String
ixFromRes xs = (takeWhile (/= '.') xs) \\ "result"

-- | Given an index of the element in the 'listVDirectory' output returns a tuple of the boundaries of the indexes usable for playback. 
-- Note: index0 is probably from [0..], l1 is necessarily from [0..]. Interesting case is: 0 <= index0 < l1.
ixInterv :: Int -> IO (Int, Int)
ixInterv = ixInterv2G "221w"

-- | Given an index of the element in the 'listVDirectory2G' (with the same 'String' as the second argument) output returns a tuple of the
-- boundaries of the indexes usable for playback. 
-- Note: index0 is probably from [0..], l1 is necessarily from [0..]. Interesting case is: 0 <= index0 < l1.
ixInterv2G :: String -> Int -> IO (Int, Int)
ixInterv2G ys index0
  | compare index0 0 == LT = do
      dirV <- listVDirectory2G ys
      let l1 = V.length dirV
      case compare l1 13 of
        LT -> return (0,l1 - 1)
        _  -> return (0,11)
  | compare index0 7 == LT = do
      dirV <- listVDirectory2G ys
      let l1 = V.length dirV
      case compare index0 (l1 - 5) of
        GT -> return (0, l1 - 1) 
        _  -> return (0, index0 + 4)     
  | otherwise = do
      dirV <- listVDirectory2G ys
      let l1 = V.length dirV
      case compare l1 13 of
       LT -> return (0,l1 - 1)
       _  -> 
         case compare index0 (l1 - 5) of
           GT -> return (index0 - 7, l1 - 1)
           _  -> return (index0 - 7, index0 + 4)           

-- | Parser to the result of 'listVDirectory2G' function to get the needed information.
infoFromV :: V.Vector String -> [(V.Vector Int, V.Vector String)]
infoFromV vec = map (internalConv . unzip . V.toList . V.map (break (== '*'))) [v1, v2]
  where (v1, v2) = V.partition shouldBeReplaced vec

-- | Plays a sequence of sounds in the interval of them obtained by 'ixInterv' function.
playSeqAR :: Int -> IO ()
playSeqAR = playSeqAR2G "221w"

-- | Generalized variant of the 'playSeqAR' with a possibility to play and mark either FLAC or WAV files. For more information, please, refer to
-- 'soxBasicParams'.
playSeqAR2G :: String -> Int -> IO ()
playSeqAR2G ys index0 = do
  (minBnd,maxBnd) <- ixInterv2G ys index0
  dirV2 <- listVDirectory2G ys
  mapM_ (playA . V.unsafeIndex dirV2) [minBnd..maxBnd]  

-- | Plays a sequence of consequential sounds in the melody in the interval of them obtained by 'ixInterv' function for each element index
-- from 'V.Vector' of indexes.
playSeqARV :: V.Vector Int -> IO ()
playSeqARV = playSeqARV2G "221w"

-- | Generalized variant of the 'playSeqARV' with a possibility to play and mark either FLAC or WAV files. For more information, please, refer to
-- 'soxBasicParams'.
playSeqARV2G :: String -> V.Vector Int -> IO ()
playSeqARV2G ys vec = do
  dirV2 <- listVDirectory2G ys
  V.mapM_ (playA . V.unsafeIndex dirV2) vec

-- | Plays a sequence of WAV sounds considered of higher quality.
playSeqARV2 :: V.Vector String -> IO ()
playSeqARV2 = playSeqARV22G "221w"

-- | Plays a sequence of sounds considered of higher quality.
playSeqARV22G :: String -> V.Vector String -> IO ()
playSeqARV22G ys vec = do
  let indexesHQs = fst . last . infoFromV $ vec  
  playSeqARV2G ys indexesHQs  

-- | The same as 'playSeqARV2', but additionally collects the resulting 'Bool' values and then returns them. It is used to define, which sounds  from those of
-- higher quality will replace those ones considered to be replaced.
playCollectDec :: V.Vector String -> IO (V.Vector Bool)
playCollectDec = playCollectDec2G "221w"

-- | Generalized variant of the 'playCollectDec' with a possibility to play and mark either FLAC or WAV files. For more information, please, refer to
-- 'soxBasicParams'.
playCollectDec2G :: String -> V.Vector String -> IO (V.Vector Bool)
playCollectDec2G ys vec = do
  dirV3 <- listVDirectory2G ys
  let indexesHQs = fst . last . infoFromV $ vec
  V.mapM (playCollect1Dec dirV3) indexesHQs  

-- | Actually replaces the file represented by 'FilePath' argument with no (then there is no replacement at all), or with just one,
-- or with a sequence of sounds being considered of higher quality to form a new melody. If the lengths of the second and the third
-- arguments differs from each other then the function uses as these arguments truncated vectors of the minimal of the two lengths. 
replaceWithHQs :: FilePath -> V.Vector Bool -> V.Vector FilePath -> IO ()
replaceWithHQs = replaceWithHQs2G "221w"

-- | Generalized variant of the 'replaceWithHQs' with a possibility to work either with FLAC files or with WAV files.
-- Please, use with the FLAC files or with the WAV files separately. Do not intend to work with both types of them simultaneously using this function. 
replaceWithHQs2G :: String -> FilePath -> V.Vector Bool -> V.Vector FilePath -> IO ()
replaceWithHQs2G ys file0 vecBools stringHQs
 | V.length vecBools == V.length stringHQs =
   case V.length stringHQs of
    0 -> putStrLn "That's all!"
    1 | V.unsafeIndex vecBools 0 -> do
         copyFile (head . V.toList $ stringHQs) ("resultI." ++ if drop 3 ys == "f" then "flac" else "wav")
         renameFile ("resultI." ++ if drop 3 ys == "f" then "flac" else "wav") file0
      | otherwise -> putStrLn "Nothing has changed. "
    _ -> do
         let yss = V.toList . V.ifilter (\i _ -> V.unsafeIndex vecBools i) $ stringHQs
         case length yss of
          0 -> putStrLn "That's all!"
          1 -> copyFile (head yss) file0
          _ -> do
            (_,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) (yss ++ soxBasicParams ys ["",file0]) ""
            putStrLn herr
 | otherwise =
  let stringHQ2s = V.take (min (V.length vecBools) (V.length stringHQs)) stringHQs
      vecBool2s  = V.take (min (V.length vecBools) (V.length stringHQs)) vecBools in replaceWithHQs2G ys file0 vecBool2s stringHQ2s

-- | 'IO' checkbox whether to add the sound played to the sequence of sounds that will replace the needed one.
thisOne :: IO Bool
thisOne = do
  putStrLn "Would you like to add this sound played just now to the sequence of sounds that will replace the needed one? "
  yes <- getLine
  putStrLn "-----------------------------------------------------------------------"
  return $ take 1 yes == "1"

-- | Plays a sound file considered to be of higher quality and then you define whether to use the played sound to replace that one considered to be replaced.
playCollect1Dec :: V.Vector String -> Int -> IO Bool
playCollect1Dec dirV2 i 
  | compare i 0 /= LT && compare i (V.length dirV2) /= GT = do 
     playA $ V.unsafeIndex dirV2 i
     thisOne
  | otherwise = error "DobutokO.Sound.IntermediateF.playCollect1Dec: wrong Int parameter! "

-- | Process the sound corresponding to the first element in the first argument. Returns a 'V.tail' of the first element of the first command line argument.
-- Replaces (if specified) the sound with a sequence of (or just one, or made no replacement at all) sounds considered of higher quality.
pAnR1 :: V.Vector String -> IO (V.Vector String)
pAnR1 = pAnR12G "221w"

-- | Generalized variant of the 'pAnR1' with a possibility to work either with FLAC files or with WAV files.
-- Please, use with the FLAC files or with the WAV files separately. Do not intend to work with both types of them simultaneously using this function. 
pAnR12G :: String -> V.Vector String -> IO (V.Vector String)
pAnR12G ys vec
 | V.null vec = putStrLn "You have processed all the marked files! " >> return V.empty
 | otherwise = do
    let [(indexes0,strings),(indexesHQ,stringHQs)] = infoFromV vec
    putStrLn "Please, listen to the melody and remember what sound you would like to replace and the surrounding sounds. "
    playSeqAR2G ys $ V.unsafeIndex indexes0 0
    putStrLn "---------------------------------------------------------------"
    putStrLn "Now, please, listen to a collection of sounds considered of higher quality which you can use to replace the needed one. "
    vecBools <- playCollectDec2G ys vec
    replaceWithHQs2G ys (V.unsafeIndex strings 0) vecBools stringHQs
    return $ V.map (\(ix,xs) -> show ix ++ "**" ++ xs) . V.zip (V.unsafeDrop 1 indexes0) $ (V.unsafeDrop 1 strings)

-- | Process the WAV sounds consequently corresponding to the elements in the first argument.
-- Replaces (if specified) the sounds with a sequence of (or just one, or made no replacement at all) sounds considered of higher quality for every sound needed.
pAnR2 :: V.Vector String -> IO ()
pAnR2 = pAnR22G "221w"

-- | Generalized variant of the 'pAnR2' with a possibility to work either with FLAC files or with WAV files.
-- Please, use with the FLAC files or with the WAV files separately. Do not intend to work with both types of them simultaneously using this function. 
pAnR22G :: String -> V.Vector String -> IO ()
pAnR22G ys vec
 | V.null vec = putStrLn "You have processed all the marked files! "
 | otherwise = onException (pAnR12G ys vec >>= pAnR22G ys) (return ())

-- | Marks the needed WAV files as of needed to be replaced or those ones considered of higher quality that will replace the needed ones. Then actually replaces them
-- as specified. Uses internally 'playAMrk' and 'pAnR2' functions. 
pAnR_ :: IO ()
pAnR_ = pAnR_2G "221w"

-- | Generalized variant of the 'pAnR_' with a possibility to work either with FLAC files or with WAV files.
-- Please, use with the FLAC files or with the WAV files separately. Do not intend to work with both types of them simultaneously using this function. 
pAnR_2G :: String -> IO ()
pAnR_2G ys = do
  vec <- playAMrk2G ys
  pAnR22G ys vec


----------------------------------------------------------------------------------------------------------------

-- | Takes a filename to be applied a SoX \"reverb" effect with parameters of list of 'String' (the second argument). Produces the temporary
-- new file with the name ((name-of-the-file) ++ (\"reverb.wav\" OR \"reverb.flac\") -- the type is preserved), which then is removed.
-- Please, remember that for the mono audio the after applied function file is stereo with 2 channels.
--
-- Besides, you can specify other SoX effects after reverberation in a list of 'String'. The syntaxis is that every separate literal must be
-- a new element in the list. If you plan to create again mono audio in the end of processment, then probably use 'reverb1E' funcion instead. 
-- If you would like to use instead of \"reverb\" its modification \"reverb -w\" effect (refer to SoX documentation), then probably it is more
-- convenient to use 'reverbWE' function. Please, check by yourself whether you have enough permissions to read and write to the 'FilePath'-specified
-- file and to the containing it directory. The function is not intended to be used in otherwise cases.
reverbE :: FilePath -> [String] -> IO ()
reverbE file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "reverb" ++ efw2 file,"reverb"] ++ arggs) ""
  case code of
    ExitSuccess -> renameFile (file ++ "reverb" ++ efw2 file) file
    _ -> do
       removeFile $ file ++ "reverb" ++ efw2 file
       putStrLn $ "DobutokO.Sound.IntermediateF.reverbE \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "

-- | Similar to 'reverbE', but replaces the primary WAV file with the new FLAC file (or vice versa). So if successful the resulting file has another
-- extension and type.
reverbE2C :: FilePath -> [String] -> IO ()
reverbE2C file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "reverb" ++ efw2vv file,"reverb"] ++ arggs) ""
  case code of
    ExitSuccess -> do { renameFile (file ++ "reverb" ++ efw2vv file) (cfw2wf file) ; removeFile file }
    _           -> do { removeFile $ file ++ "reverb" ++ efw2vv file
                      ; putStrLn $ "DobutokO.Sound.IntermediateF.reverbE2C \"" ++ file ++ "\" has not been successful. The file has not been changed at all. " }

-- | The same as 'reverbE', but at the end file is being mixed to obtain mono audio. The name of the temporary file is ((name-of-the-file) ++
--  (\"reverb1.wav\" OR \"reverb1.flac\") -- the type is preserved).
-- Please, check by yourself whether you have enough permissions to read and write to the 'FilePath'-specified
-- file and to the containing it directory. The function is not intended to be used in otherwise cases.
reverb1E :: FilePath -> [String] -> IO ()
reverb1E file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "reverb1" ++ efw2 file,"reverb"] ++ arggs ++ ["channels","1"]) ""
  case code of
    ExitSuccess -> renameFile (file ++ "reverb1" ++ efw2 file) file
    _ -> do
       removeFile $ file ++ "reverb1" ++ efw2 file
       putStrLn $ "DobutokO.Sound.IntermediateF.reverb1E \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "

-- | Similar to 'reverb1E', but replaces the primary WAV file with the new FLAC file (or vice versa). So if successful the resulting file has another
-- extension and type.
reverb1E2C :: FilePath -> [String] -> IO ()
reverb1E2C file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "reverb1" ++ efw2vv file,"reverb"] ++ arggs ++ ["channels","1"]) ""
  case code of
    ExitSuccess -> do { renameFile (file ++ "reverb1" ++ efw2vv file) (cfw2wf file) ; removeFile file }
    _ -> do
       removeFile $ file ++ "reverb1" ++ efw2vv file
       putStrLn $ "DobutokO.Sound.IntermediateF.reverb1E2C \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "       

-- | The same as 'reverbE', but uses \"reverb -w\" effect instead of \"reverb\". The name of the temporary file is
-- ((name-of-the-file) ++ (\"reverbW.wav\" OR \"reverbW.flac\") -- the type is preserved). Please, for more information, refer to SoX documentation.
-- Please, check by yourself whether you have enough permissions to read and write to the 'FilePath'-specified
-- file and to the containing it directory. The function is not intended to be used in otherwise cases.
reverbWE :: FilePath -> [String] -> IO ()
reverbWE file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "reverbW" ++ efw2 file,"reverb","-w"] ++ arggs) ""
  case code of
    ExitSuccess -> renameFile (file ++ "reverbW" ++ efw2 file) file
    _ -> do
       removeFile $ file ++ "reverbW" ++ efw2 file
       putStrLn $ "DobutokO.Sound.IntermediateF.reverbWE \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "

-- | Similar to 'reverbWE', but replaces the primary WAV file with the new FLAC file (or vice versa). So if successful the resulting file has another
-- extension and type.
reverbWE2C :: FilePath -> [String] -> IO ()
reverbWE2C file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "reverbW" ++ efw2vv file,"reverb","-w"] ++ arggs) ""
  case code of
    ExitSuccess -> do { renameFile (file ++ "reverbW" ++ efw2vv file) (cfw2wf file) ; removeFile file }
    _           -> do { removeFile $ file ++ "reverbW" ++ efw2vv file
                      ; putStrLn $ "DobutokO.Sound.IntermediateF.reverbWE2C \"" ++ file ++ "\" has not been successful. The file has not been changed at all. " }

-- | The same as 'reverbWE', but at the end file is being mixed to obtain mono audio. The name of the temporary file is ((name-of-the-file)
-- ++ (\"reverbW1.wav\" OR \"reverbW1.flac\") -- the type is preserved). Please, check by yourself whether you have enough permissions
-- to read and write to the 'FilePath'-specified file and to the containing it directory. The function is not intended to be used in otherwise cases.
reverbW1E :: FilePath -> [String] -> IO ()
reverbW1E file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "reverbW1" ++ efw2 file,"reverb","-w"] ++ arggs ++ ["channels","1"]) ""
  case code of
    ExitSuccess -> renameFile (file ++ "reverbW1" ++ efw2 file) file
    _ -> do
       removeFile $ file ++ "reverbW1" ++ efw2 file
       putStrLn $ "DobutokO.Sound.IntermediateF.reverbW1E \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "

-- | Similar to 'reverb1WE', but replaces the primary WAV file with the new FLAC file (or vice versa). So if successful the resulting file has another
-- extension and type.
reverb1WE2C :: FilePath -> [String] -> IO ()
reverb1WE2C file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "reverbW1" ++ efw2vv file,"reverb","-w"] ++ arggs ++ ["channels","1"]) ""
  case code of
    ExitSuccess -> do { renameFile (file ++ "reverbW1" ++ efw2vv file) (cfw2wf file) ; removeFile file }
    _ -> do
       removeFile $ file ++ "reverbW1" ++ efw2vv file
       putStrLn $ "DobutokO.Sound.IntermediateF.reverb1WE2C \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "       

-- | Is used internally in the functions to specify different SoX parameters for the sound synthesis (rate, bit depth and file extension). Possible
-- file extensions are: ".wav" (a default one) and ".flac" (being lossless compressed); rates -- 8000, 11025, 16000, 22050 (a default one), 32000,
--  44100, 48000, 88200, 96000, 176400, 192000 Hz; bit depths -- 16 bits and 24 bits. The first two digits in a 'String' argument encodes rate,
-- the next one -- bit depth and the last symbol -- letter \'w\' or \'f\' -- file extension. Because of SoX uses FLAC optionally, before use it, please,
-- check whether your installation supports it.
soxBasicParams :: String -> [String] -> [String]
soxBasicParams ys xss 
 | null xss = []
 | otherwise =
    let (ts,zs) = splitAt 2 . init $ ys in (getBFst' ("-r22050",listArray (0,8) . zip ["11","16", "17", "19", "32", "44", "48", "80", "96"] $
      ["-r11025","-r16000","-r176400","-r192000","-r32000","-r44100","-r48000","-r8000","-r96000"]) ts) : (if zs == "2" then "-b24" else "-b16") :
        ((if drop 3 ys == "f" then map (\xs -> if drop (length xs - 4) xs == ".wav" then take (length xs - 4) xs ++ ".flac" else xs) else id) . tail $ xss)

-- | Similar to 'soxE', but replaces the primary WAV file with the new FLAC file (or vice versa). So if successful the resulting file has another
-- extension and type.
soxE2C :: FilePath -> [String] -> IO ()
soxE2C file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "effects" ++ efw2vv file] ++ arggs) ""
  case code of
    ExitSuccess -> do { renameFile (file ++ "effects" ++ efw2vv file) (cfw2wf file) ; removeFile file }
    _ -> do
       removeFile $ file ++ "effects" ++ efw2vv file
       putStrLn $ "DobutokO.Sound.IntermediateF.soxE2C \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "       

-- | The same as 'soxE', but at the end file is being mixed to obtain mono audio.
-- Please, check by yourself whether you have enough permissions to read and write to the 'FilePath'-specified
-- file and to the containing it directory. The function is not intended to be used in otherwise cases.
soxE1 :: FilePath -> [String] -> IO ()
soxE1 file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "effects" ++ efw2 file] ++ arggs ++ ["channels","1"]) ""
  case code of
    ExitSuccess -> renameFile (file ++ "effects" ++ efw2 file) file
    _ -> do
       removeFile $ file ++ "effects" ++ efw2 file
       putStrLn $ "DobutokO.Sound.IntermediateF.soxE1 \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "

-- | Similar to 'soxE1', but replaces the primary WAV file with the new FLAC file (or vice versa). So if successful the resulting file has another
-- extension and type.
soxE12C :: FilePath -> [String] -> IO ()
soxE12C file arggs = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) ([file,file ++ "effects" ++ efw2vv file] ++ arggs ++ ["channels","1"]) ""
  case code of
    ExitSuccess -> do { renameFile (file ++ "effects" ++ efw2vv file) (cfw2wf file) ; removeFile file }
    _ -> do
       removeFile $ file ++ "effects" ++ efw2vv file
       putStrLn $ "DobutokO.Sound.IntermediateF.soxE12C \"" ++ file ++ "\" has not been successful. The file has not been changed at all. "

-- | Function takes a 'FilePath' for the new recorded file (if it already exists then it is overwritten) and a list of 'String'. The last one is
-- sent to SoX rec or something equivalent as its arguments after the filename. If you plan just afterwards to produce mono audio, it's simpler to use
-- 'rec1E' function instead. Please, check by yourself whether you have enough permissions to read and write to the 'FilePath'-specified
-- file and to the containing it directory. The function is not intended to be used in otherwise cases.
-- Function is adopted and changed 'Sound.SoXBasics.recA' function.
recE :: FilePath -> [String] -> IO ()
recE file arggs | isJust (showE "sox") && take 5 os == "mingw" = do 
  (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) (["-t","waveaudio","-d", file] ++ arggs)""
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
  (code, _, _) <- readProcessWithExitCode (fromJust (showE "rec")) (file:arggs) ""
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

-- | Function takes a 'FilePath' for the new recorded file (if it already exists then it is overwritten) and a list of 'String'. The last one is
-- sent to SoX rec or something equivalent as its arguments after the filename. Please, check by yourself whether you have enough permissions
-- to read and write to the 'FilePath'-specified file and to the containing it directory. The function is not intended to be used in otherwise cases.
-- Function is adopted and changed 'Sound.SoXBasics.recA' function.
rec1E :: FilePath -> [String] -> IO ()
rec1E file arggs | isJust (showE "sox") && take 5 os == "mingw" = do 
  (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) (["-t","waveaudio","-d", file] ++ arggs ++ ["channels","1"])""
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
  (code, _, _) <- readProcessWithExitCode (fromJust (showE "rec")) ([file] ++ arggs ++ ["channels","1"]) ""
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

-- | Plays a 'FilePath' file with a SoX further effects specified by the list of 'String'. It can be e. g. used to (safely) test the result of applying
-- some SoX effects and only then to use 'soxE' or some similar functions to actually apply them.
-- Please, check by yourself whether you have enough permissions to read the 'FilePath'-specified
-- file and the containing it directory. The function is not intended to be used in otherwise cases.
-- Function is adopted and changed 'Sound.SoXBasics.playA' function.
playE :: FilePath -> [String] -> IO ()
playE file arggs | take 5 os == "mingw" = 
  if isJust (showE "sox") 
    then void (readProcessWithExitCode (fromJust (showE "sox")) ([file, "-t", "waveaudio", "-d"] ++ arggs) "")
    else catchEnd ExecutableNotProperlyInstalled
                 | otherwise = if isJust (showE "play") 
  then void (readProcessWithExitCode (fromJust (showE "play")) ([file] ++ arggs) "")
  else catchEnd ExecutableNotProperlyInstalled

-- | Changes the volume of the given 'FilePath' with supported by SoX sound file type so that it becomes 0 (zero). Makes so it a silence file with the
-- same parameters for duration, rate, bit depth and file type.
getSilenceF :: FilePath -> IO ()
getSilenceF file = soxE file ["vol","0"]

-- | Applies 'fadeEnds' to all the \"zs*.wav\" (or instead all the \"zs*.flac\") files in the current directory. The file extension
-- is defined by the first 'String' argument in accordance with 'soxBasicParams'. @zs@ here is given by the second 'String' argument.
fadeAllE :: String -> String -> IO ()
fadeAllE ys zs = listVDirectory3G ys zs >>= V.mapM_ fadeEnds

-- | Applies 'fadeEndsMilN' to all the \"zs*.wav\" (or instead all the \"zs*.flac\") files in the current directory. The file extension
-- is defined by the first 'String' argument in accordance with 'soxBasicParams'. @zs@ here is given by the second 'String' argument. The 'Int' argument 
-- defines a number of miliseconds to be under fading effect (no more than 10).
fadeAllEMilN :: Int -> String -> String -> IO ()
fadeAllEMilN n ys zs = listVDirectory3G ys zs >>= V.mapM_ (fadeEndsMilN n) 

-------------------------------------------------------------------------------------------  

-- | A predicate to decide whether an element @a@ belongs to the odd number of the lists of @a@ in the 'V.Vector'. 
isOddAsElem :: Eq a => a -> V.Vector [a] -> Bool
isOddAsElem x v
  | V.null v = False
  | otherwise = (V.length . V.findIndices (elem x) $ v) `rem` 2 == 1

-- | All @[a]@ must be finite. To obtain @Just a0@ as a result, at least one of the @[a]@ must be not empty and 'V.Vector' must have finite length.
-- If 'V.Vector' is 'V.empty' or all @[a]@ are null (the vector has finite length), then the result is 'Nothing'. Otherwise, it will run infinitely
-- just until it runs over the available memory.
maxLinV :: Ord a => V.Vector [a] -> Maybe a
maxLinV v
  | V.all null v  || V.null v = Nothing
  | otherwise = Just (V.maximum . V.map maximum . V.filter (not . null) $ v)

-- | All @[a]@ must be finite. To obtain @Just a0@ as a result, at least one of the @[a]@ must be not empty and 'V.Vector' must have finite length.
-- If 'V.Vector' is 'V.empty' or all @[a]@ are null (the vector has finite length), then the result is 'Nothing'. Otherwise, it will run infinitely
-- just until it runs over the available memory.
minLinV :: Ord a => V.Vector [a] -> Maybe a
minLinV v
  | V.all null v || V.null v = Nothing
  | otherwise = Just (V.minimum . V.map minimum . V.filter (not . null) $ v)

-- | Applied to list of @[a]@ where a is an instance for 'Ord' class gives a sorted in the ascending order 'V.Vector' of @a@, each of them being unique.
doubleLtoV :: Ord a => [[a]] -> V.Vector a
doubleLtoV = V.fromList . shortenL . L.sort . concat
   where shortenL z1@(z:_)
          | length (takeWhile (== z) z1) `rem` 2 == 1 = z:shortenL (dropWhile (== z) z1)
          | otherwise = shortenL (dropWhile (== z) z1)
         shortenL _ = []

-- | Filters 'Int' elements in a list so that they are limited with the first two 'Int' arguments of the function as a lower and a higher bounds.
filterToBnds :: Int -> Int -> [Int] -> [Int]
filterToBnds lbnd hbnd = filter (\x -> compare x lbnd /= LT && compare x hbnd /= GT)

-- | Applies a special chain of the SoX effects to a file to obtain a somewhat similar to some instruments sound for some values of the 'Int' parameters. 
-- These last ones are used (after some normalizing transformation) as the arguments for the SoX \"reverb -w\" effect. For more information about their 
-- meaning, please, refer to the SoX and reverberation documentation, besides you can give them a try. 
soxREw1 :: Int -> Int -> Int -> Int -> Int -> Int -> FilePath -> IO ()
soxREw1 reverberance damping roomscale stereodepth predelay wetgain file = do 
  durat <- durationA file
  soxE file (concat [["channels", "2", "rate", "44100", "reverb", "-w"], map (\n -> show (abs n `rem` 101)) [reverberance, damping, roomscale, stereodepth], 
    [show (abs predelay `rem` 501), show (abs wetgain `rem` 7), "trim", "0", showFFloat (Just 5) durat "", "reverse", "fade", "q", "0.002", "-0.0", "earwax"]])

-- | Applies a special chain of the SoX effects to a file to obtain a somewhat other its sounding. Similar to 'soxREw1' in realization, but can give 
-- rather another sounding. 
soxRE1 :: Int -> Int -> Int -> Int -> Int -> Int -> FilePath -> IO ()
soxRE1 reverberance damping roomscale stereodepth predelay wetgain file = do 
  durat <- durationA file
  soxE file (concat [["channels", "2", "rate", "44100", "reverb"], map (\n -> show (abs n `rem` 101)) [reverberance, damping, roomscale, stereodepth],  
    [show (abs predelay `rem` 501), show (abs wetgain `rem` 7), "trim", "0", showFFloat (Just 5) durat "", "reverse", "fade", "q", "0.002", "-0.0", "earwax"]])

-- | Applies a special chain of the SoX effects to the files which are obtained as a result of the 'listVDirectory3G' in the current directory. 
-- For some values of the first six 'Int' parameters you obtain somewhat similar to some instruments sounds. 
-- These parameters are used (after some normalizing transformation) as the arguments for the SoX \"reverb -w\" effect. For more information about their 
-- meaning, please, refer to the SoX and reverberation documentation, besides you can give them a try. The last 'Int' parameter is the first argument 
-- for the afterwards general SoX "reverb" effect. 'String' arguments are that ones for the 'listVDirectory3G'. The 'FilePath' argument is a name 
-- for the resulting file (in the supported by the SoX format). 
soxREA1 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> String -> FilePath -> IO ()
soxREA1 reverberance damping roomscale stereodepth predelay wetgain reverb2 ys zs file = do 
  dir0V <- listVDirectory3G ys zs
  V.mapM_ (soxREw1 reverberance damping roomscale stereodepth predelay wetgain) dir0V
  (_,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) (concat [V.toList dir0V, [file, "reverb", show (abs reverb2 `rem` 101)]]) ""
  print herr
  
