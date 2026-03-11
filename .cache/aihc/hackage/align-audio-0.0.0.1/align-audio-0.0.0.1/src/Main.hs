module Main where

import qualified CorrelateResample
import qualified Correlate
import qualified Option

import qualified Synthesizer.Basic.Binary as Bin
import qualified Data.StorableVector.Lazy as SVL
import qualified Sound.SoxLib as SoxLib
import Foreign.Storable (peek)

import qualified Options.Applicative as OP
import Shell.Utility.Exit (exitFailureMsg)

import Text.Printf (printf)

import Control.Monad (when)
import Control.Applicative (liftA2, (<*>))
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe)


withFirstChannel ::
   FilePath ->
   (Double -> SVL.Vector Float -> IO ()) ->
   IO ()
withFirstChannel src act =
   SoxLib.withRead SoxLib.defaultReaderInfo src $ \fmtInPtr -> do
      fmtIn <- peek fmtInPtr
      let numChan = fromMaybe 1 $ SoxLib.channels $ SoxLib.signalInfo fmtIn
      rate <-
         case SoxLib.rate $ SoxLib.signalInfo fmtIn of
            Nothing -> exitFailureMsg "no sample rate found"
            Just rate -> return rate
      act rate .
         SVL.sieve numChan . SVL.map Bin.toCanonical =<<
         SoxLib.readStorableVectorLazy fmtInPtr (SVL.ChunkSize 65536)

maybeInteger :: Double -> Maybe Integer
maybeInteger nf =
   let n = round nf
   in toMaybe (nf == fromInteger n) n

absLag :: Int -> (String, Int)
absLag n =
   if n >= 0
      then ("second", n)
      else ("first", -n)


main :: IO ()
main = SoxLib.formatWith $ do
   opt <-
      OP.execParser $
      OP.info (OP.helper <*> Option.parseFlags) Option.desc
   withFirstChannel (Option.input0 opt) $ \rate0 audio0 ->
      withFirstChannel (Option.input1 opt) $ \rate1 audio1 ->
      case liftA2 (,) (maybeInteger rate0) (maybeInteger rate1) of
         Nothing -> do
            when (rate0/=rate1) $
               exitFailureMsg "sample rates are fractional and differ"
            let (ref,lag) = absLag $ Correlate.determineLag audio0 audio1
            printf
               "you must delay the %s source by %d samples at %f, i.e. %.6f seconds\n"
               ref lag rate0 (fromIntegral lag / rate0)
         Just (irate0,irate1) -> do
            let dstRate = Option.rate opt
            let (ref,lag) = absLag $
                  CorrelateResample.determineLag dstRate
                     (irate0,audio0) (irate1,audio1)
            printf
               "you must delay the %s source by %d samples at %d, i.e. %.3f seconds\n"
               ref lag dstRate (fromIntegral lag / fromInteger dstRate :: Double)
