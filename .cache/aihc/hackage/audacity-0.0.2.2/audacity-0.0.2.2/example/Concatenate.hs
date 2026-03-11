module Main where

import Common (defaultSampleRate, multiArgs, withSound, writerInfoFromFormat, )

import qualified Sound.Audacity.LabelTrack as LabelTrack
import qualified Sound.SoxLib as SoxLib

import qualified Data.StorableVector.Lazy as SVL

import Control.Monad (when, liftM2, )

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import Data.Traversable (forM, )
import Data.Maybe (fromMaybe, )

import qualified System.FilePath as FilePath
import System.FilePath ((<.>), )
import Text.Printf (printf, )



run :: NonEmpty.T [] FilePath -> FilePath -> IO ()
run neInputs@(NonEmpty.Cons input0 inputs) output = do
   let write fmtOut numChan sig = do
         SoxLib.writeStorableVectorLazy fmtOut sig
         return $! div (SVL.length sig) numChan
   (rate, lengths) <-
      withSound input0 $ \fmtIn rate0 numChan0 sig0 ->
      SoxLib.withWrite (writerInfoFromFormat fmtIn) output $ \fmtOut ->
      fmap ((,) rate0) $
      liftM2 NonEmpty.Cons
         (write fmtOut numChan0 sig0)
         (forM inputs $ \input ->
            withSound input $ \ _fmtIn rate numChan sig -> do
               let showRate = maybe "<no rate>" show
               when (rate0 /= rate) $
                  ioError $ userError $
                     printf "%s: rate %s differs from initial rate %s"
                        input (showRate rate) (showRate rate0)
               when (numChan0 /= numChan) $
                  ioError $ userError $
                     printf "%s: number channels (%d) differs from initial input (%d)"
                        input numChan numChan0
               write fmtOut numChan sig)
   LabelTrack.writeFileInt
         (fromMaybe defaultSampleRate rate)
         (FilePath.dropExtension output <.> "txt") $
      LabelTrack.fromAdjacentChunks $
      NonEmpty.flatten $ NonEmptyC.zip lengths $
      fmap FilePath.takeBaseName neInputs


main :: IO ()
main =
   SoxLib.formatWith $
   uncurry run =<< multiArgs "Concatenate audio files and track origins"
