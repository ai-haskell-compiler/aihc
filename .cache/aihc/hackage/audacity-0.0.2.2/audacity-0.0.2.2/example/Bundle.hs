module Main where

import Common
         (defaultSampleRate, multiArgs, withSound, summaryName, waveSummary, )

import qualified Sound.Audacity.Project.Track.Label as ProjectLabelTrack
import qualified Sound.Audacity.Project.Track.Wave.Summary as ProjectWaveSummary
import qualified Sound.Audacity.Project.Track.Wave as ProjectWaveTrack
import qualified Sound.Audacity.Project as Audacity
import qualified Sound.Audacity.LabelTrack as LabelTrack
import qualified Sound.SoxLib as SoxLib

import Control.Monad (when, liftM2, )

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.Traversable (forM, )

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
run neInputs@(NonEmpty.Cons input0 inputs) output = do
   let outputSummary = summaryName output
   let defltRate maybeRate = maybe defaultSampleRate id maybeRate
   let write sh maybeRate numChan input sig = do
         sequs <- waveSummary sh numChan input sig
         dur <-
            return $!
               fromIntegral
                  (maximum $ map ProjectWaveTrack.numSamples_ sequs)
                     / defltRate maybeRate
         return (sequs, dur)
   (maybeRate, sequLengths) <-
      ProjectWaveSummary.withHandle outputSummary $ \sh ->
      withSound input0 $ \ _fmt rate0 numChan0 sig0 ->
      fmap ((,) rate0) $
      liftM2 (:) (write sh rate0 numChan0 input0 sig0) $
         forM inputs $ \input ->
         withSound input $ \ _fmt rate numChan sig -> do
            let showRate = maybe "<no rate>" show
            when (rate0 /= rate) $
               ioError $ userError $
                  printf "%s: rate %s differs from initial rate %s"
                     input (showRate rate) (showRate rate0)
            when (numChan0 /= numChan) $
               ioError $ userError $
                  printf "%s: number channels (%d) differs from initial input (%d)"
                     input numChan numChan0
            write sh rate numChan input sig
   let (sequss, lengths) = unzip sequLengths
       originTrack =
         LabelTrack.fromAdjacentChunks $
         zip lengths $ map FilePath.takeBaseName $ NonEmpty.flatten neInputs
       sequenceTracks =
         flip map (List.transpose sequss) $ \sequs ->
         Audacity.WaveTrack $
         ProjectWaveTrack.deflt {
            ProjectWaveTrack.name_ = "bundle",
            ProjectWaveTrack.rate_ = round $ defltRate maybeRate,
            ProjectWaveTrack.clips_ =
               zipWith ProjectWaveTrack.Clip (scanl (+) 0 lengths) sequs
         }

   writeFile output $
      flip Audacity.format "" $
      Audacity.deflt {
         Audacity.zoom_ = 850 / sum lengths,
         Audacity.name_ = FilePath.takeBaseName outputSummary,
         Audacity.tracks_ =
            sequenceTracks ++ [projectLabelTrack "origin" originTrack]
      }


main :: IO ()
main =
   SoxLib.formatWith $
   uncurry run =<< multiArgs "Create Audacity track from multiple clips"
