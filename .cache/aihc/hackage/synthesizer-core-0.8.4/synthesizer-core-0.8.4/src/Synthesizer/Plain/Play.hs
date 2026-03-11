{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Plain.Play where

import qualified Synthesizer.Plain.Builder as Builder
import qualified Synthesizer.Basic.Binary as BinSmp

import qualified Sound.Sox.Frame as Frame
import qualified Sound.Sox.Frame.Stereo as Stereo
import qualified Sound.Sox.Option.Format as SoxOpt
import qualified Sound.Sox.Play as Play
import qualified Sound.Sox.Signal.List as SoxList

import Foreign.Storable (Storable, )
import Data.Int (Int16, )
import Data.Monoid (mconcat, )

import System.Exit (ExitCode, )

import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.RealRing as RealRing

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
See 'Synthesizer.Plain.File.write'.
-}
render ::
   (Storable int, Frame.C int, ToInteger.C int, Bounded int,
    RealRing.C a, BinSmp.C v) =>
   Builder.Put int -> a -> (a -> [v]) -> IO ExitCode
render put sampleRate renderer =
   auto put sampleRate (renderer sampleRate)

renderToInt16 :: (RealRing.C a, BinSmp.C v) => a -> (a -> [v]) -> IO ExitCode
renderToInt16 sampleRate renderer =
   toInt16 sampleRate (renderer sampleRate)

renderMonoToInt16 :: (RealRing.C a) => a -> (a -> [a]) -> IO ExitCode
renderMonoToInt16 sampleRate renderer =
   monoToInt16 sampleRate (renderer sampleRate)

renderStereoToInt16 :: (RealRing.C a) => a -> (a -> [(a,a)]) -> IO ExitCode
renderStereoToInt16 sampleRate renderer =
   stereoToInt16 sampleRate (renderer sampleRate)


{- |
See 'Synthesizer.Plain.File.write'.
-}
auto ::
   (Storable int, Frame.C int, ToInteger.C int, Bounded int,
    RealRing.C a, BinSmp.C v) =>
   Builder.Put int -> a -> [v] -> IO ExitCode
auto put sampleRate signal =
   raw
      (SoxOpt.numberOfChannels (BinSmp.numberOfSignalChannels signal))
      sampleRate
      (Builder.run . mconcat . map (BinSmp.outputFromCanonical put) $
       signal)

toInt16 :: (RealRing.C a, BinSmp.C v) => a -> [v] -> IO ExitCode
toInt16 =
   auto (Builder.put :: Builder.Put Int16)

monoToInt16 :: (RealRing.C a) => a -> [a] -> IO ExitCode
monoToInt16 sampleRate signal =
   raw SoxOpt.none sampleRate
      (map BinSmp.int16FromCanonical signal)

stereoToInt16 :: (RealRing.C a) => a -> [(a,a)] -> IO ExitCode
stereoToInt16 sampleRate signal =
   raw SoxOpt.none sampleRate
      (map (fmap BinSmp.int16FromCanonical . uncurry Stereo.cons) signal)


raw :: (RealRing.C a, Frame.C v, Storable v) =>
   SoxOpt.T -> a -> [v] -> IO ExitCode
raw opts sampleRate signal =
   Play.extended SoxList.put opts SoxOpt.none (round sampleRate) signal


exampleMono :: IO ExitCode
exampleMono =
   monoToInt16 (11025::Double) (map sin [0::Double,0.2..])

exampleStereo :: IO ExitCode
exampleStereo =
   stereoToInt16 (11025::Double) $
      zip
         (map sin [0::Double,0.199..])
         (map sin [0::Double,0.201..])
