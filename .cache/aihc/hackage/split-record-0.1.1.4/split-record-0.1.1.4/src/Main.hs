module Main where

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.ChunkySize.Cut as CutCS
import qualified Synthesizer.ChunkySize as ChunkySize
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.State.Cut as Cut
import qualified Synthesizer.State.Signal as Sig
import qualified Synthesizer.Basic.Binary as Bin

import qualified Sound.SoxLib as SoxLib

import qualified Data.StorableVector.Lazy as SVL
import Foreign.Storable (peek, )

import qualified Control.Monad.Trans.State as MS
import Control.Monad (when, )
import Control.Arrow (arr, (<<<), (^<<), )

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (swap, )
import Data.Foldable (forM_, )
import Data.Maybe (fromMaybe, )

import qualified System.Console.GetOpt as Opt
import System.Console.GetOpt
          (getOpt, usageInfo, ArgDescr(NoArg, ReqArg), )
import System.Environment (getArgs, getProgName, )
import Text.Printf (printf, )

import qualified System.Exit as Exit
import Shell.Utility.Exit (exitFailureMsg)

import qualified Algebra.RealRing as Real
import NumericPrelude.Numeric
import NumericPrelude.Base

import Data.Int (Int32, )

import Prelude ()


-- * parameters

newtype Time = Time Float
   deriving (Eq, Show)

newtype Freq = Freq Float
   deriving (Eq, Show)

data Flags =
   Flags {
      flagComputeEnvelope :: Bool,
      flagSampleRate :: Maybe SoxLib.Rate,
      flagSmooth, flagHumFreq :: Freq,
      flagPauseVolume :: Float,
      flagMinPause, flagPreStart :: Time,
      flagBlocksize :: SVL.ChunkSize
   }

defltFlags :: Flags
defltFlags =
   Flags {
      flagComputeEnvelope = False,
      flagSampleRate = Nothing,
      flagSmooth = Freq 1,
      flagHumFreq = Freq 100,
      flagPauseVolume = 0.02,
      flagMinPause = Time 2,
      {-
      Sometimes a piece starts with breath which is almost undetectable.
      Thus we start a little bit earlier than necessary.
      -}
      -- flagPreStart = Time 1.5,
      flagPreStart = Time 0.05,
      flagBlocksize = SVL.chunkSize 65536
   }


data Params =
   Params {
      sampleRate :: SoxLib.Rate,
      numChannels :: Int,
      smooth, humFreq :: Float,
      pauseVolume :: Float,
      minPause, preStart :: Int
   }


defaultSampleRate :: SoxLib.Rate
defaultSampleRate = 44100

freq :: SoxLib.Rate -> (Flags -> Freq) -> (Flags -> Float)
freq sr acc flags =
   (case acc flags of Freq f -> f) / realToFrac sr

time :: SoxLib.Rate -> (Flags -> Time) -> (Flags -> Int)
time sr acc flags =
   round ((case acc flags of Time t -> t) * realToFrac sr)

formatFreq :: Freq -> String
formatFreq (Freq t) = show t -- ++ "Hz"

formatTime :: Time -> String
formatTime (Time t) = show t -- ++ "s"


parseCard :: (Read a, Real.C a) => String -> String -> IO a
parseCard name str =
   case reads str of
      [(n,"")] ->
         case compare n zero of
           GT -> return n
           EQ -> exitFailureMsg $ name ++ " must not be zero"
           LT -> exitFailureMsg $ "negative " ++ name ++ ": " ++ str
      _ -> exitFailureMsg $ "could not parse " ++ name ++ " " ++ show str

numberArg ::
   (Read a, Real.C a) =>
   String -> (a -> Flags -> IO Flags) ->
   Opt.ArgDescr (Flags -> IO Flags)
numberArg name update =
   flip ReqArg name $ \str flags ->
      flip update flags =<< parseCard name str

description :: [ Opt.OptDescr (Flags -> IO Flags) ]
description =
    Opt.Option ['h'] ["help"]
        (NoArg $ \ _flags -> do
           programName <- getProgName
           putStrLn $
              usageInfo ("Usage: " ++ programName ++ " [OPTIONS] INPUT [OUTPUT]") $
              description
           Exit.exitSuccess)
        "show options" :
    Opt.Option ['r'] ["rate"]
        (numberArg "SAMPLERATE" $ \n flags ->
           return $ flags{flagSampleRate = Just n})
        ("sample rate, default " ++ show defaultSampleRate) :
    Opt.Option [] ["pause-volume"]
        (numberArg "AMPLITUDE" $ \n flags ->
           return $ flags{flagPauseVolume = n})
        ("required maximum amplitude in pauses between pieces, default " ++ show (flagPauseVolume defltFlags)) :
    Opt.Option [] ["smooth"]
        (numberArg "FREQUENCY" $ \n flags ->
           return $ flags{flagSmooth = Freq n})
        ("cutoff frequency for smoothing envelope, default " ++ formatFreq (flagSmooth defltFlags)) :
    Opt.Option [] ["hum-frequency"]
        (numberArg "FREQUENCY" $ \n flags ->
           return $ flags{flagHumFreq = Freq n})
        ("cutoff frequency for hum elimination, default " ++ formatFreq (flagHumFreq defltFlags)) :
    Opt.Option [] ["min-pause"]
        (numberArg "TIME" $ \n flags ->
           return $ flags{flagMinPause = Time n})
        ("minimal required pause between pieces, default " ++ formatTime (flagMinPause defltFlags)) :
    Opt.Option [] ["pre-start"]
        (numberArg "TIME" $ \n flags ->
           return $ flags{flagPreStart = Time n})
        ("time to start before threshold is exceeded, default " ++ formatTime (flagPreStart defltFlags)) :
    Opt.Option [] ["blocksize"]
        (numberArg "NUMSAMPLES" $ \n flags ->
           return $ flags{flagBlocksize = SVL.chunkSize n})
        ("size of processing chunks, default " ++
         case flagBlocksize defltFlags of SVL.ChunkSize size -> show size) :
    Opt.Option [] ["compute-envelope"]
        (NoArg $ \ flags -> do
           return $ flags{flagComputeEnvelope = True})
        "compute envelope for assistance in finding appropriate parameters" :
    []


-- * computation

dehum :: Params -> Causal.T Float Float
dehum params =
   Filt1.highpass_
   ^<<
   Filt1.causal
   <<<
   Causal.feedConstFst (Filt1.parameter (humFreq params))

trackEnvelope :: Params -> [SVL.Vector Float] -> SVL.Vector Float
trackEnvelope params =
   Causal.apply
      (Filt1.lowpassCausal
       <<<
       Causal.feedConstFst (Filt1.parameter (smooth params))
       <<<
       arr sqrt)
   .
   foldl SigSt.mix SVL.empty

threshold :: Params -> Causal.T Float Bool
threshold params = Causal.map (< pauseVolume params)

findStarts :: Params -> Causal.T Bool Bool
findStarts params =
   flip Causal.fromState 0 $ \b ->
      if b
        then MS.modify succ >> evalReturn False
        else do n <- MS.get; MS.put 0; return (n >= minPause params)

measurePauses :: Causal.T Bool (Maybe Int)
measurePauses =
   flip Causal.fromState 0 $ \b ->
      if b
        then do n <- MS.get; MS.put 1; return (Just n)
        else MS.modify succ >> evalReturn Nothing

evalReturn :: a -> MS.State Int a
evalReturn x =
   MS.gets (\n -> seq n x)

pieceDurations :: Params -> SVL.Vector Int32 -> [Int]
pieceDurations params =
--   catMaybes . Sig.toList .
   Sig.foldR (maybe id (:)) [] .
   Causal.apply
      (measurePauses <<< findStarts params <<< threshold params) .
   Sig.fromStorableSignal .
   trackEnvelope params .
   map (Causal.apply (arr (^2) <<< dehum params <<< arr Bin.toCanonical)) .
   SVL.deinterleave (numChannels params)

pieceDurationsPrefetchLazy :: Params -> SVL.Vector Int32 -> [ChunkySize.T]
pieceDurationsPrefetchLazy params sig =
   flip Cut.chopChunkySize (CutCS.length sig) .
   flip Sig.append (Sig.repeat False) .
   Sig.drop (preStart params) .
   Causal.apply (findStarts params <<< threshold params) .
   Sig.fromStorableSignal .
   trackEnvelope params .
   map (Causal.apply (arr (^2) <<< dehum params <<< arr Bin.toCanonical)) .
   SVL.deinterleave (numChannels params) $ sig


prefetch :: Int -> [Int] -> [Int]
prefetch _ [] = []
prefetch n (s:ss) =
   if s <= n
     then prefetch (n-s) ss
     else (s-n) : ss

chop, chopLazy ::
   Params -> SVL.Vector Int32 -> [SVL.Vector Int32]
chop params sig0 =
   snd $
   List.mapAccumL (\sig n -> swap $ SVL.splitAt n sig) sig0 $
   map (numChannels params *) .
   prefetch (preStart params) $ pieceDurations params sig0

chopLazy params sig0 =
   snd $
   List.mapAccumL (\sig n -> swap $ CutCS.splitAt n sig) sig0 $
   map (fromIntegral (numChannels params) *) .
   pieceDurationsPrefetchLazy params $ sig0



-- * driver

withSound ::
   Flags -> FilePath ->
   (SoxLib.Format SoxLib.ReadMode ->
    Params -> SVL.Vector Int32 -> IO b) ->
   IO b
withSound flags path act =
   SoxLib.withRead SoxLib.defaultReaderInfo path $ \fmtPtr -> do
      fmt <- peek fmtPtr
      let numChan =
             fromMaybe 1 $ SoxLib.channels $ SoxLib.signalInfo fmt
          rate =
             case flagSampleRate flags of
                Just r -> r
                Nothing ->
                   case SoxLib.rate $ SoxLib.signalInfo fmt of
                      Just r -> r
                      Nothing -> defaultSampleRate
          params =
             Params {
                sampleRate = rate,
                numChannels = numChan,
                smooth = freq rate flagSmooth flags,
                humFreq = freq rate flagHumFreq flags,
                pauseVolume = flagPauseVolume flags,
                minPause = time rate flagMinPause flags,
                preStart = time rate flagPreStart flags
             }
      act fmt params =<<
         SoxLib.readStorableVectorLazy fmtPtr
            (case flagBlocksize flags of
               SVL.ChunkSize size -> SVL.ChunkSize $ numChan * size)


monoInfoFromFormat ::
   SoxLib.Format mode -> Params -> SoxLib.WriterInfo
monoInfoFromFormat fmtIn params =
   SoxLib.defaultWriterInfo {
      SoxLib.writerSignalInfo = Just $
         (SoxLib.signalInfo fmtIn) {
            SoxLib.channels = Just 1,
            SoxLib.rate = Just $ sampleRate params
         },
      SoxLib.writerEncodingInfo = Just $ SoxLib.encodingInfo fmtIn
   }

writerInfoFromFormat ::
   SoxLib.Format mode -> Params -> SoxLib.WriterInfo
writerInfoFromFormat fmtIn params =
   SoxLib.defaultWriterInfo {
      SoxLib.writerSignalInfo = Just $
         (SoxLib.signalInfo fmtIn) {
            SoxLib.rate = Just $ sampleRate params
         },
      SoxLib.writerEncodingInfo = Just $ SoxLib.encodingInfo fmtIn
   }

runDehum :: Flags -> FilePath -> FilePath -> IO ()
runDehum flags input output =
   withSound flags input $ \fmtIn params sig ->
   SoxLib.withWrite
      (writerInfoFromFormat fmtIn params)
      output $ \fmtOut ->
         SoxLib.writeStorableVectorLazy fmtOut $
         SVL.interleaveFirstPattern $
         map
            (Causal.apply
               (arr (Bin.fromCanonicalWith Real.roundSimple)
                <<<
                dehum params
                <<<
                arr Bin.toCanonical)) $
         SVL.deinterleave (numChannels params) sig

runEnvelope :: Flags -> FilePath -> FilePath -> IO ()
runEnvelope flags input output =
   withSound flags input $ \fmtIn params sig ->
   SoxLib.withWrite
      (monoInfoFromFormat fmtIn params)
      output $ \fmtOut ->
         SoxLib.writeStorableVectorLazy fmtOut $
         Causal.apply
            (arr (Bin.fromCanonicalWith Real.roundSimple)) $
         trackEnvelope params $
         map
            (Causal.apply
               (arr (^2)
                <<<
                dehum params
                <<<
                arr Bin.toCanonical)) $
         SVL.deinterleave (numChannels params) sig

runSizes :: Flags -> FilePath -> IO ()
runSizes flags input =
   withSound flags input $ \_fmt params sig ->
   mapM_ print $ pieceDurations params sig

runLabels :: Flags -> FilePath -> IO ()
runLabels flags input =
   withSound flags input $ \_fmt params sig ->
   mapM_ (\(n, (from,to)) -> printf "%s\t%s\t%d\n" from to n) $
   zip [(0::Int) ..] $
   ListHT.mapAdjacent (,) $
   map (\t ->
      case divMod (Real.roundSimple (fromIntegral t * 10^6 / sampleRate params)) (10^6) of
         -- FIXME: the comma is certainly only correct in German locale
         (seconds,micros) ->
            printf "%d,%06d" seconds (micros::Integer) :: String) $
   scanl (+) 0 $
   prefetch (preStart params) $
   pieceDurations params sig

{- |
> runChop flags "in.wav" "%03d.wav"
-}
runChop :: Flags -> FilePath -> FilePath -> IO ()
runChop flags input output =
   withSound flags input $ \fmtIn params sig ->
   forM_ (zip [(0::Int)..] $ chopLazy params sig) $ \(n,piece) ->
      SoxLib.withWrite
         (writerInfoFromFormat fmtIn params)
         (printf output n) $ \fmtOut ->
            SoxLib.writeStorableVectorLazy fmtOut piece


main :: IO ()
main = SoxLib.formatWith $ do
   argv <- getArgs
   let (opts, files, errors) =
          getOpt Opt.RequireOrder description argv
   when (not $ null errors) $
      exitFailureMsg (init (concat errors))

   flags <- foldl (>>=) (return defltFlags) opts

   if flagComputeEnvelope flags
     then
       case files of
          [input,output] -> runEnvelope flags input output
          [] -> exitFailureMsg "need input and output file envelope computation"
          _ -> exitFailureMsg "more than two file names given"
     else
       case files of
          [input,output] -> runChop flags input output
          [input] -> runLabels flags input
          [] -> exitFailureMsg "no input or output given"
          _ -> exitFailureMsg "more than two file names given"
