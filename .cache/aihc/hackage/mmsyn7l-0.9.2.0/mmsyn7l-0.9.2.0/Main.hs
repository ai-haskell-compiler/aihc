-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020-2022, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A program and a library to modify the amplitudes of the sound representations 
-- created by mmsyn7ukr or mmsyn7ukr-array package or somehow otherwise. Besides
-- it can be used to adjust volume for the sequential \"result*.wav\" files.
--

module Main where

import Control.Exception (onException)
import MMSyn7l (changeVolume,adjustVolRes)
import CaseBi.Arr (getBFstL')
import System.Environment (getArgs)
import Parser.ReplaceP (replaceP, replaceP4)

-- | The main and the only one function in the module. If you specify a one command line argument, which can be obtained by running @mmsyn7s@ program
-- with the needed to be sounded (with sound representations) Ukrainian text (please, refer to: https://hackage.haskell.org/package/mmsyn7s).
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Now you can change the amplitude of the sound representations of the Ukrainian sounds (or something similar). "
      putStrLn ""
      mapM_ (changeVolume 1) ["A.wav", "B.wav", "C.wav", "D.wav", "E.wav", "F.wav", "G.wav", "H.wav", 
        "I.wav", "J.wav", "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav", 
          "S.wav", "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", "a.wav", "b.wav", "c.wav", 
            "d.wav", "e.wav", "f.wav"]
      putStrLn ""
      putStrLn "Now you have changed (or left unchanged) the amplitudes for the sound representations for Ukrainian language. "
      putStrLn "Please, remember about responsible usage especially in case of processing the real voice sound samples! "
    ("-h":_)  -> do
      putStrLn "SYNOPSYS: "
      putStrLn "mmsyn7l -h    OR"
      putStrLn "mmsyn7l -v    OR"
      putStrLn "mmsyn7l -r [number-of-the-first-file-to-be-volume-adjusted [number-of-the-last-file-to-be-volume-adjusted]]   OR"
      putStrLn "mmsyn7l [list-of-needed-sounds-to-be-amplitude-modified]"
      putStrLn "\"-h\" prints this message."
      putStrLn "\"-v\" prints version number of the program."
      putStr "\"-r\" works with the \"result*.wav\" files in the current directory: it adjusts a volume for the sequence of them starting from the "
      putStr "first argument and ending with the second (if specified). If there is no first -- all such files are adjusted; if there is no second one -- "
      putStrLn "the files are adjusted to the last one. Count starts at 0. "
      putStr "[list-of-needed-sounds-to-be-amplitude-modified (if given) -- the program modifies the amplitudes in the interactive mode only for the "
      putStrLn "given sound representations. If not specified, the program modifies the amplitudes for all non-pause Ukrainian sounds representations."
      putStrLn ""
    ("-v":_)  -> do
      putStrLn "mmsyn7l version: 0.9.2.0"
      putStrLn ""
    ("-r":_)  -> adjustVolRes . drop 1 $ args
    _         ->  do
      putStrLn "Now you can change the amplitude of the needed sound representations of the Ukrainian sounds (or something similar). "
      putStrLn ""
      let wss = read (replaceP4 . show $ (read  (replaceP . show . take 1 $ args)::[String]))::[String]
      mapM_ (changeVolume 1) . map (getBFstL' "0.wav" (zip ["а","б","в","г","д","дж","дз","е","ж","з","и","й","к","л","м","н","о","п","р","с",
        "сь","т","у","ф","х","ц","ць","ч","ш","ь","і","ґ"] ["A.wav", "B.wav", "C.wav", "D.wav", "E.wav", "F.wav", "G.wav", "H.wav", "I.wav", "J.wav", 
          "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav", "S.wav", "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", 
            "a.wav", "b.wav", "c.wav", "d.wav", "e.wav", "f.wav"])) $ wss
      putStrLn ""
      putStrLn "Now you have changed (or left unchanged) the amplitudes for the needed sound representations for the Ukrainian language. "
      putStrLn "Please, remember about responsible usage especially in case of processing the real voice sound samples! "

            
