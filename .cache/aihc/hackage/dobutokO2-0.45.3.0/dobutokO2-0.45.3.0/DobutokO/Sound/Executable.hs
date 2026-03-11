-- |
-- Module      :  DobutokO.Sound.Executable
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Executable (
  -- * Basic functions for the executable
  dobutokO2
  , dobutokO2H7
  , dobutokO2H9
  , dobutokO2H92
  , o2help
) where

import Control.Monad (void)
import System.Exit (ExitCode (ExitSuccess))
import qualified Data.List as L (groupBy,sort)
import CaseBi.Arr (getBFstLSorted')
import Control.Exception (onException)
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust,fromMaybe)
import System.Process
import EndOfExe2 (showE)
import qualified Data.Vector as V (Vector,generate,fromList,length,imapM_,snoc,toList,unsafeSlice,mapM_,unsafeIndex)
import System.Directory
import DobutokO.Sound.Overtones
import DobutokO.Sound.Uniq
import DobutokO.Sound.IntermediateF 
import DobutokO.Sound.ParseList (parseStoLInts)
import DobutokO.Sound.Process

-- | Function that actually makes processing in the @dobutokO2@ executable. Please, check before executing
-- whether there is no \"x.wav\", \"test*\", \"result*\" and \"end*\" files in the current directory, because they can be overwritten.
dobutokO2 :: IO ()
dobutokO2 = do
  arggs <- getArgs
  let arg1 = concat . take 1 $ arggs
      file = concat . drop 1 . take 2 $ arggs
      args = unwords . drop 2 $ arggs
      argss = drop 1 arggs
  exist2 <- doesFileExist file
  getBFstLSorted' (dobutokO2H exist2 args file) (fmap (\(xs, f) -> (xs,f exist2 args file)) $ [("0",o2help),("00",dobutokO2H00),
    ("002",dobutokO2H002),("1",dobutokO2H1),("11",dobutokO2H11),("2",dobutokO2H2),("21",dobutokO2H21),("3",dobutokO2H3),("31",dobutokO2H31),
      ("4",dobutokO2H4),("41",dobutokO2H41),("5",dobutokO2H5),("51",dobutokO2H51),("61",dobutokO2H61),("7",dobutokO2H7),("8",dobutokO2H8),
        ("80",dobutokO2H80),("82",dobutokO2H82),("820",dobutokO2H820),("9",dobutokO2H9),("92",dobutokO2H92),("99",dobutokO2H99 argss),
          ("992",dobutokO2H992G argss),("999",dobutokO2H999 argss),("9992",dobutokO2H9992G argss)]) arg1

dobutokO2H1 :: Bool -> String -> FilePath -> IO ()
dobutokO2H1 exist2 args file = do
  [_,_,octave,ampLS,time2] <- mapM (recAndProcess file) (if exist2 then [0,2,3,4,5] else [1..5])
  let (octave1,ampL,time3) = fromJust . threeStr2Val $ [octave,ampLS,time2] in overSoXSynthNGen file octave1 ampL time3 args
{-# INLINE dobutokO2H1 #-}

threeStr2Val :: [String] -> Maybe (Int,Float,Float)
threeStr2Val [xs,ys,zs] = Just (read xs::Int,read ys::Float,read zs::Float)
threeStr2Val _ = Nothing
{-# INLINE threeStr2Val #-}

fourStr2Val :: [String] -> Maybe (Int,Float,Float,Float)
fourStr2Val [xs,ys,zs,tws] = Just (read xs::Int,read ys::Float,read zs::Float,read tws::Float)
fourStr2Val _ = Nothing
{-# INLINE fourStr2Val #-}

fiveStr2Val :: [String] -> Maybe ([Int],Float,Float,Float)
fiveStr2Val [xs,ys,zs,tws] = Just (map (\z -> read z::Int) . words $ xs,read ys::Float,read zs::Float,read tws::Float)
fiveStr2Val _ = Nothing
{-# INLINE fiveStr2Val #-}

dobutokO2H2 :: Bool -> String -> FilePath -> IO ()
dobutokO2H2 exist2 args file = do
  [_,_,octave,ampLS,time2,wws] <- mapM (recAndProcess file) (if exist2 then [0,2,3,4,5,6] else [1..6])
  let (octave1,ampL,time3) = fromJust . threeStr2Val $ [octave,ampLS,time2] in uniqOverSoXSynthNGen file octave1 ampL time3 args wws
{-# INLINE dobutokO2H2 #-}

dobutokO2H3 :: Bool -> String -> FilePath -> IO ()
dobutokO2H3 exist2 args file = do
  [_,_,octave,ampLS,time2,tts] <- mapM (recAndProcess file) (if exist2 then [0,2,3,4,5,7] else [1,2,3,4,5,7])
  let (octave1,ampL,time3) = fromJust . threeStr2Val $ [octave,ampLS,time2] in overSoXSynthNGen2 file octave1 ampL time3 args tts
{-# INLINE dobutokO2H3 #-}

dobutokO2H4 :: Bool -> String -> FilePath -> IO ()
dobutokO2H4 exist2 args file = do
  [_,_,octave,ampLS,time2,wws,tts] <- mapM (recAndProcess file) (if exist2 then [0,2,3,4,5,6,7] else [1..7])
  let (octave1,ampL,time3) = fromJust . threeStr2Val $ [octave,ampLS,time2] in uniqOverSoXSynthNGen3 file octave1 ampL time3 args wws tts
{-# INLINE dobutokO2H4 #-}

dobutokO2H5 :: Bool -> String -> FilePath -> IO ()
dobutokO2H5 exist2 args file = do
  [_,_,octave,ampLS,time2,tts,dAmpl0,vs] <- mapM (recAndProcess file) (if exist2 then [0,2,3,4,5,7,8,9] else [1,2,3,4,5,7,8,9])
  let (octave1,ampL,time3,dAmpl) = fromJust . fourStr2Val $ [octave,ampLS,time2,dAmpl0] in overSoXSynthNGen3 file octave1 ampL time3 dAmpl args tts vs
{-# INLINE dobutokO2H5 #-}

dobutokO2H :: Bool -> String -> FilePath -> IO ()
dobutokO2H exist2 args file = do
  [_,_,octave,ampLS,time2,wws,tts,dAmpl0,vs] <- mapM (recAndProcess file) (if exist2 then [0,2,3,4,5,6,7,8,9] else [1..9])
  let (octave1,ampL,time3,dAmpl) = fromJust . fourStr2Val $ [octave,ampLS,time2,dAmpl0] in
    uniqOverSoXSynthNGen4 file octave1 ampL time3 dAmpl args wws tts vs
{-# INLINE dobutokO2H #-}

dobutokO2H61 :: Bool -> String -> FilePath -> IO ()
dobutokO2H61 exist2 args file = do
  [_,_,complexNky,ampLS,time2,wws,tts,dAmpl0,vs] <- mapM (recAndProcess file) (if exist2 then [0,2,11,4,5,6,7,8,9] else [1,2,11,4,5,6,7,8,9])
  let ([enkA,nTh],ampL,time3,dAmpl) = fromJust . fiveStr2Val $ [complexNky,ampLS,time2,dAmpl0] in
    uniqOverSoXSynthNGen4E file nTh enkA ampL time3 dAmpl args wws tts vs
{-# INLINE dobutokO2H61 #-}

dobutokO2H8 :: Bool -> String -> FilePath -> IO ()
dobutokO2H8 exist2 args file = void (dobutokO2H8G exist2 args file)
{-# INLINE dobutokO2H8 #-}

dobutokO2H80 :: Bool -> String -> FilePath -> IO ()
dobutokO2H80 exist2 args file = dobutokO2H8G exist2 args file  >>= \case
    (ExitSuccess, path8v) -> V.mapM_ removeFile path8v
    _           -> return ()
{-# INLINE dobutokO2H80 #-}

dobutokO2H8G :: Bool -> String -> FilePath -> IO (ExitCode, V.Vector FilePath)
dobutokO2H8G _ _ _ = do
  path8s0 <- listDirectory "."
  let path8v = V.fromList . L.sort . filter (isPrefixOf "result") $ path8s0
      path8v1 = V.generate (V.length path8v `quot` 800) (\i0 -> V.unsafeSlice (i0 * 800) 800 path8v ) `V.snoc` V.unsafeSlice (800 *
         (V.length path8v `quot` 800)) (V.length path8v `rem` 800) path8v 
  V.imapM_ dO2H8 path8v1
  epath0s <- listDirectory "."
  let epaths = L.sort . filter (isPrefixOf "end0") $ epath0s
  (code1,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) (epaths ++ ["end.wav"]) ""
  case code1 of
    ExitSuccess -> mapM_ removeFile epaths
    _           -> do
      exi1 <- doesFileExist "end.wav"
      if exi1
        then do
          removeFile "end.wav"
          error "The end file \"end.wav\" was not created. "
        else error "The end file \"end.wav\" was not created. "
  return (code1, path8v)
{-# INLINE dobutokO2H8G #-}

dobutokO2H82G :: Bool -> String -> FilePath -> IO (ExitCode, V.Vector FilePath)
dobutokO2H82G _ ys _ = do
  path8s0 <- listDirectory "."
  let path8v = V.fromList . L.sort . filter (isPrefixOf "result") $ path8s0
      path8v1 = V.generate (V.length path8v `quot` 800) (\i0 -> V.unsafeSlice (i0 * 800) 800 path8v ) `V.snoc` V.unsafeSlice (800 *
         (V.length path8v `quot` 800)) (V.length path8v `rem` 800) path8v 
  if drop 3 ys == "f" then V.imapM_ dO2H8f path8v1 else V.imapM_ dO2H8 path8v1
  epath0s <- listDirectory "."
  let epaths = L.sort . filter (isPrefixOf "end0") $ epath0s
  (code1,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) (epaths ++ soxBasicParams ys ("":["end.wav"])) ""
  case code1 of
    ExitSuccess -> mapM_ removeFile epaths
    _           -> do
      exiW <- doesFileExist "end.wav"
      exiF <- doesFileExist "end.flac"
      if exiW && not (exiF)
        then do
          removeFile "end.wav"
          error "The end file \"end.wav\" was not created. "
        else
          if exiF && not (exiW) then do
            removeFile "end.flac"
            error "The end file \"end.flac\" was not created. "
          else if exiF && exiW then do
                 removeFile "end.flac"
                 removeFile "end.wav"
                 error "The end file \"end.*\" was not created. "
               else error "The end file \"end.*\" was not created. "
  return (code1, path8v)
{-# INLINE dobutokO2H82G #-}

dobutokO2H82 :: Bool -> String -> FilePath -> IO ()
dobutokO2H82 exist2 ys file = void (dobutokO2H82G exist2 ys file)
{-# INLINE dobutokO2H82 #-}

dobutokO2H820 :: Bool -> String -> FilePath -> IO ()
dobutokO2H820 exist2 ys file = dobutokO2H82G exist2 ys file  >>= \case
    (ExitSuccess, path8v) -> V.mapM_ removeFile path8v
    _           -> return ()
{-# INLINE dobutokO2H820 #-}

dobutokO2H11 :: Bool -> String -> FilePath -> IO ()
dobutokO2H11 exist2 args file = do
  [_,_,complexNky,ampLS,time2] <- mapM (recAndProcess file) (if exist2 then [0,2,11,4,5] else [1,2,11,4,5])
  let ([enkA,nTh],ampL,time3,_) = fromJust . fiveStr2Val $ [complexNky,ampLS,time2,""] in overSoXSynthNGenE file nTh enkA ampL time3 args
{-# INLINE dobutokO2H11 #-}

dobutokO2H21 :: Bool -> String -> FilePath -> IO ()
dobutokO2H21 exist2 args file = do
  [_,_,complexNky,ampLS,time2,wws] <- mapM (recAndProcess file) (if exist2 then [0,2,11,4,5,6] else [1,2,11,4,5,6])
  let ([enkA,nTh],ampL,time3,_) = fromJust . fiveStr2Val $ [complexNky,ampLS,time2,""] in uniqOverSoXSynthNGenE file nTh enkA ampL time3 args wws
{-# INLINE dobutokO2H21 #-}

dobutokO2H31 :: Bool -> String -> FilePath -> IO ()
dobutokO2H31 exist2 args file = do
  [_,_,complexNky,ampLS,time2,tts] <- mapM (recAndProcess file) (if exist2 then [0,2,11,4,5,7] else [1,2,11,4,5,7])
  let ([enkA,nTh],ampL,time3,_) = fromJust . fiveStr2Val $ [complexNky,ampLS,time2,""] in overSoXSynthNGen2E file nTh enkA ampL time3 args tts
{-# INLINE dobutokO2H31 #-}

dobutokO2H41 :: Bool -> String -> FilePath -> IO ()
dobutokO2H41 exist2 args file = do
  [_,_,complexNky,ampLS,time2,wws,tts] <- mapM (recAndProcess file) (if exist2 then [0,2,11,4,5,6,7] else [1,2,11,4,5,6,7])
  let ([enkA,nTh],ampL,time3,_) = fromJust . fiveStr2Val $ [complexNky,ampLS,time2,""] in uniqOverSoXSynthNGen3E file nTh enkA ampL time3 args wws tts
{-# INLINE dobutokO2H41 #-}

dobutokO2H51 :: Bool -> String -> FilePath -> IO ()
dobutokO2H51 exist2 args file = do
  [_,_,complexNky,ampLS,time2,tts,dAmpl0,vs] <- mapM (recAndProcess file) (if exist2 then [0,2,11,4,5,7,8,9] else [1,2,11,4,5,7,8,9])
  let ([enkA,nTh],ampL,time3,dAmpl) = fromJust . fiveStr2Val $ [complexNky,ampLS,time2,dAmpl0] in
    overSoXSynthNGen3E file nTh enkA ampL time3 dAmpl args tts vs
{-# INLINE dobutokO2H51 #-}

dO2H8 :: Int -> V.Vector String -> IO ()
dO2H8 i v = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) (V.toList v ++ ["end0" ++ show i ++ ".wav"]) ""
  case code of
    ExitSuccess -> putStr ""
    _           -> do
      exi0 <- doesFileExist $ "end0" ++ show i ++ ".wav"
      if exi0
        then do
          removeFile $ "end0" ++ show i ++ ".wav"
          error $ "The intermediate file " ++ "\"end0" ++ show i ++ ".wav\" was not created. "
        else error $ "The intermediate file " ++ "\"end0" ++ show i ++ ".wav\" was not created. "
{-# INLINE dO2H8 #-}

dO2H8f :: Int -> V.Vector String -> IO ()
dO2H8f i v = do
  (code,_,_) <- readProcessWithExitCode (fromJust (showE "sox")) (V.toList v ++ ["end0" ++ show i ++ ".flac"]) ""
  case code of
    ExitSuccess -> putStr ""
    _           -> do
      exi0 <- doesFileExist $ "end0" ++ show i ++ ".flac"
      if exi0
        then do
          removeFile $ "end0" ++ show i ++ ".flac"
          error $ "The intermediate file " ++ "\"end0" ++ show i ++ ".flac\" was not created. "
        else error $ "The intermediate file " ++ "\"end0" ++ show i ++ ".flac\" was not created. "
{-# INLINE dO2H8f #-}

-- | Actually works as 'pAnR_' function.
dobutokO2H9 :: Bool -> String -> FilePath -> IO ()
dobutokO2H9 _ _ _ = pAnR_
{-# INLINE dobutokO2H9 #-}

-- | Actually works as 'pAnR_2G' function.
dobutokO2H92 :: Bool -> String -> FilePath -> IO ()
dobutokO2H92 _ ys _ = pAnR_2G ys
{-# INLINE dobutokO2H92 #-}

dobutokO2H99 :: [String] -> Bool -> String -> FilePath -> IO ()
dobutokO2H99 argss _ _ file = do
  (v1,dir0V) <- dO2H99 "221w" file 
  V.mapM_ (\idx -> playE (V.unsafeIndex dir0V idx) argss) v1
{-# INLINE dobutokO2H99 #-}

dO2H99 :: String -> FilePath -> IO (V.Vector Int,V.Vector FilePath)
dO2H99 ys file = do
  dir0V <- listVDirectory2G ys
  let l0 = V.length dir0V
  putStrLn $ "You have available " ++ show l0 ++ " files that can be played. The minimum index further is 0, the maximum is " ++ show (l0 - 1)
  list1 <- recAndProcess file (99::Int)
  let yss = divideToStr l0 list1
      v01 = V.fromList yss
      mxE = fromMaybe (l0 - 1) (maxLinV v01)
      mnE = fromMaybe 0 (minLinV v01)
      zss = map (filterToBnds mnE mxE) yss
      v1 = doubleLtoV zss
  return (v1,dir0V)
{-# INLINE dO2H99 #-}

dobutokO2H992G :: [String] -> Bool -> String -> FilePath -> IO ()
dobutokO2H992G argss _ ys file = do
  (v1,dir0V) <- dO2H99 ys file 
  V.mapM_ (\idx -> playE (V.unsafeIndex dir0V idx) argss) v1
{-# INLINE dobutokO2H992G #-}

dobutokO2H999 :: [String] -> Bool -> String -> FilePath -> IO ()
dobutokO2H999 argss _ _ file = do
  (v1,dir0V) <- dO2H99 "221w" file 
  V.mapM_ (\idx -> soxE1 (V.unsafeIndex dir0V idx) argss) v1
{-# INLINE dobutokO2H999 #-}

dobutokO2H9992G :: [String] -> Bool -> String -> FilePath -> IO ()
dobutokO2H9992G argss _ ys file = do
  (v1,dir0V) <- dO2H99 ys file 
  V.mapM_ (\idx -> soxE1 (V.unsafeIndex dir0V idx) argss) v1
{-# INLINE dobutokO2H9992G #-}

divideToStr :: Int -> String -> [[Int]]
divideToStr n = map (parseStoLInts n). lines

isDataStr :: String -> Bool
isDataStr = notElem '@'

isTextPair :: String -> String -> Bool
isTextPair xs ys = isDataStr xs && isDataStr ys

-- | Used to obtain one multiline specially formatted textual input and do the full processment for the sound. 
-- The function generates overtones using additional 'String' and allows maximum control over the parameters.
-- Besides, all the needed information it obtains from the singular formatted input, which can be ended
-- with a keyboard keys combination that means an end of input (e. g. for Unices, that is probably Ctrl + D).
-- \'@\' are separators for the input parts for their respective parts. For more information about the
-- format of the single input, see:
--
-- 'https://drive.google.com/open?id=10Z_GRZR4TKoL5KXfqPm-t-4humuHN0O4'
--
-- The file is also provided with the package as text.dat.txt. 
-- The last two or three inputs (an input just here means a textual input between two \'@\') can be omitted,
-- the program will work also but with less control for the user possible. 
-- 
dobutokO2H7 :: Bool -> String -> FilePath -> IO ()
dobutokO2H7 True args file = do
  putStrLn "Please, specify a prepared textual input. To end the input press a keyboard keys combination that means an end of the input (e. g. for Unices, possibly Ctrl + D). "
  input <- getContents
  let text0   = lines input
      listTxt = filter isDataStr . map (unwords . words . unlines) . L.groupBy isTextPair $ text0
      l       = length listTxt
  case l of
    4 -> onException (do
      let [octave0,ampLS0,time20,wws] = listTxt
          octave1 = read (d3H octave0)::Int
          ampL = read (d4H ampLS0)::Float
          time3 = read (d5H time20)::Float
      uniqOverSoXSynthNGen file octave1 ampL time3 args wws) (do
        putStrLn "--------------------------------------------------------------------------------------------------------------------"
        putStrLn ""
        putStrLn "The operation was not successful because of the not valid textual input. Please, specify a valid textual input. "
        dobutokO2H7 True args file)
    5 -> onException (do
      let [octave0,ampLS0,time20,wws,tts0] = listTxt
          octave1 = read (d3H octave0)::Int
          ampL = read (d4H ampLS0)::Float
          time3 = read (d5H time20)::Float
      uniqOverSoXSynthNGen3 file octave1 ampL time3 args wws (d7H tts0)) (do
        putStrLn "--------------------------------------------------------------------------------------------------------------------"
        putStrLn ""
        putStrLn "The operation was not successful because of the not valid textual input. Please, specify a valid textual input. "
        dobutokO2H7 True args file)
    7 -> onException (do
      let [octave0,ampLS0,time20,wws,tts0,dAmpl0,vs0] = listTxt
          octave1 = read (d3H octave0)::Int
          ampL = read (d4H ampLS0)::Float
          time3 = read (d5H time20)::Float
          dAmpl = read (d8H dAmpl0)::Float
      uniqOverSoXSynthNGen4 file octave1 ampL time3 dAmpl args wws (d7H tts0) (d9H vs0)) (do
        putStrLn "--------------------------------------------------------------------------------------------------------------------"
        putStrLn ""
        putStrLn "The operation was not successful because of the not valid textual input. Please, specify a valid textual input. "
        dobutokO2H7 True args file)
    _ -> do
        putStrLn "--------------------------------------------------------------------------------------------------------------------"
        putStrLn ""
        putStrLn "The operation was not successful because of the not valid textual input. Please, specify a valid textual input. "
        dobutokO2H7 True args file
dobutokO2H7 _ args file = onException (do
  _ <- processD1
  _ <- processD2 file
  dobutokO2H7 True args file) (do
    putStrLn "--------------------------------------------------------------------------------------------------------------------"
    putStrLn ""
    putStr "The operation was not successful because the file with such a name does not exist or was not created by a program. "
    putStrLn "Please, interrupt a program and start again with a better data. "
    dobutokO2H7 False args file)
{-# INLINE dobutokO2H7 #-}

-- | Takes textual input from the stdin and prints it as one 'String' to the stdout.
o2help :: Bool -> String -> FilePath -> IO ()
o2help _ _ _ = do
  xs <- getContents
  let ys = unwords . lines $ xs in do
    putStrLn ""
    putStrLn "-------------------------------------------------------------------------------------------------------------"
    putStrLn ys

dobutokO2H00 :: Bool -> String -> FilePath -> IO ()
dobutokO2H00 _ = fadeAllE
{-# INLINE dobutokO2H00 #-}

dobutokO2H002 :: Bool -> String -> FilePath -> IO ()
dobutokO2H002 _ = fadeAllEMilN 2
{-# INLINE dobutokO2H002 #-}
