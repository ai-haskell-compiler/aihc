module Main where

import Common (defaultSampleRate, info, withSound, writerInfoFromFormat)

import qualified Sound.Audacity.LabelTrack as LabelTrack
import qualified Sound.SoxLib as SoxLib

import qualified Data.StorableVector.Lazy as SVL

import qualified Options.Applicative as OP
import Control.Monad (forM_)

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Tuple.HT (uncurry3)
import Data.Ord.HT (comparing)
import Data.Char (isDigit)
import Data.Int (Int32)

import Text.Printf (printf)


formatName :: String -> Int -> String -> String
formatName fmt k label =
   let go ('%':cs0) =
         case cs0 of
            's':cs1 -> label ++ go cs1
            '%':cs1 -> '%' : go cs1
            _ ->
               case span isDigit cs0 of
                  (digits,'d':cs2) -> printf ("%"++digits++"d") k ++ go cs2
                  _ -> '%' : go cs0
       go (c:cs) = c : go cs
       go [] = []
   in  go fmt

split ::
   LabelTrack.T Int a -> Int -> SVL.Vector Int32 ->
   [(a, SVL.Vector Int32)]
split labels numChan sig =
   snd $
   List.mapAccumL
      (\(tOld,sigOld) ((t0,t1),label) ->
         let sigNew = SVL.drop (numChan*(t0-tOld)) sigOld
         in  ((t0,sigNew), (label, SVL.take (numChan*(t1-t0)) sigNew)))
      (0, sig) $
   List.sortBy (comparing fst) $ LabelTrack.decons labels

run :: FilePath -> FilePath -> FilePath -> IO ()
run labelFile input output = do
   labels <- LabelTrack.readFile labelFile
   withSound input $ \fmtIn mrate numChan sig -> do
      let rate = fromMaybe defaultSampleRate mrate
          pieces =
            split (LabelTrack.mapTime (\t -> round (t*rate)) labels) numChan sig
      forM_ (zip [0..] pieces) $ \(k,(label,piece)) ->
         SoxLib.withWrite
            (writerInfoFromFormat fmtIn)
            (formatName output k label)
            (flip SoxLib.writeStorableVectorLazy piece)

parseArgs :: OP.Parser (FilePath, FilePath, FilePath)
parseArgs =
   OP.liftA3 (,,)
      (OP.strArgument (OP.metavar "LABELTRACK"))
      (OP.strArgument (OP.metavar "INPUT"))
      (OP.strOption
         (OP.long "output" <> OP.metavar "FMT" <> OP.value "%s.wav"
            <> OP.help "include %s for labels and %03d for counts"))

main :: IO ()
main =
   SoxLib.formatWith $
   uncurry3 run =<<
      OP.execParser
         (info "Split audio file according to label track" parseArgs)
