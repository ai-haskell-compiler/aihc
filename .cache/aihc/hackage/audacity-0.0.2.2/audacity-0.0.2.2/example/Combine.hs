module Main where

import Common
         (defaultSampleRate, multiArgs, withSound, summaryName, waveSummary, )

import qualified Sound.Audacity.Project.Track.Label as ProjectLabelTrack
import qualified Sound.Audacity.Project.Track.Wave.Summary as ProjectWaveSummary
import qualified Sound.Audacity.Project.Track.Wave as ProjectWaveTrack
import qualified Sound.Audacity.Project as Audacity
import qualified Sound.Audacity.LabelTrack as LabelTrack
import qualified Sound.SoxLib as SoxLib

import qualified Control.Functor.HT as FuncHT
import Control.Monad (join, )

import qualified Data.NonEmpty as NonEmpty
import Data.Traversable (forM, )
import Data.NonEmpty ((!:), )

import qualified System.FilePath as FilePath
import Text.Printf (printf, )



projectLabelTrack :: String -> LabelTrack.T Double String -> Audacity.Track
projectLabelTrack name labels =
   Audacity.LabelTrack
      (ProjectLabelTrack.deflt {
         ProjectLabelTrack.name_ = name,
         ProjectLabelTrack.track_ = labels
      })

run :: NonEmpty.T [] FilePath -> FilePath -> IO ()
run inputs output = do
   let outputSummary = summaryName output
   let defltRate maybeRate = maybe defaultSampleRate id maybeRate
   let write sh maybeRate numChan input sig = do
         let chanExc =
               ioError $ userError $
                  printf "%s: number channels (%d) is too small" input numChan
         sequs <-
            (maybe chanExc return . NonEmpty.fetch =<<) $
            waveSummary sh numChan input sig
         return $ flip fmap sequs $ \sequ ->
            (Audacity.WaveTrack $
             ProjectWaveTrack.deflt {
               ProjectWaveTrack.name_ = FilePath.takeBaseName input,
               ProjectWaveTrack.rate_ = round $ defltRate maybeRate,
               ProjectWaveTrack.clips_ = [ProjectWaveTrack.Clip 0 sequ]
             },
             fromIntegral (ProjectWaveTrack.numSamples_ sequ)
               / defltRate maybeRate)

   trackLengths <-
      ProjectWaveSummary.withHandle outputSummary $ \sh ->
      forM inputs $ \input ->
      if FilePath.takeExtensions input == ".txt"
         then do
            labels <- LabelTrack.readFile input
            return $ NonEmpty.singleton $
               (projectLabelTrack (FilePath.takeBaseName input) labels,
                NonEmpty.maximum $ (0!:) $
                map (snd.fst) $ LabelTrack.decons labels)
         else
            withSound input $ \ _fmt rate numChan sig ->
               write sh rate numChan input sig
   let (tracks, lengths) = FuncHT.unzip $ join trackLengths

   writeFile output $
      flip Audacity.format "" $
      Audacity.deflt {
         Audacity.zoom_ = 850 / NonEmpty.maximum lengths,
         Audacity.name_ = FilePath.takeBaseName outputSummary,
         Audacity.tracks_ = NonEmpty.flatten tracks
      }


main :: IO ()
main =
   SoxLib.formatWith $
   uncurry run =<< multiArgs "Create Audacity project from multiple tracks"
