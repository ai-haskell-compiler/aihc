{-# LANGUAGE RebindableSyntax #-}
{- |
Play audio signals via ALSA.
The module could also be called @Output@,
because with a @file@ sink, data can also be written to disk.
-}
module Synthesizer.ALSA.Storable.Play (
   -- * auxiliary functions
   Device,
   defaultDevice,
   defaultChunkSize,
   makeSink,
   write,
   writeLazy,
   -- * play functions
   auto,
   autoAndRecord,
   autoAndRecordMany,
   monoToInt16,
   stereoToInt16,
   ) where

import qualified Sound.ALSA.PCM as ALSA

import qualified Synthesizer.Frame.Stereo as Stereo
import qualified Synthesizer.Basic.Binary as BinSmp

import qualified Sound.Sox.Frame         as SoxFrame
import qualified Sound.Sox.Write         as SoxWrite
import qualified Sound.Sox.Option.Format as SoxOption

import Foreign.Storable (Storable, )
import Foreign.Marshal.Array (advancePtr, )
import Foreign.Ptr (Ptr, minusPtr, )
import Data.Int (Int16, )
import qualified System.IO as IO
import qualified System.Exit as Exit

-- import qualified Synthesizer.State.Signal     as SigS

import qualified Synthesizer.Storable.Signal     as SigSt
import qualified Data.StorableVector.Lazy        as SVL
import qualified Data.StorableVector.Base        as SVB

import qualified Algebra.RealRing as RealRing

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
A suggested default chunk size.
It is not used by the functions in this module.
-}
{-
Better move to Storable.Server.Common or Dimensional.Server.Common?
-}
defaultChunkSize :: SigSt.ChunkSize
defaultChunkSize = SigSt.chunkSize 512
{-
At some epochs this chunk size leads to buffer underruns.
I cannot reproduce this:
Some months it works this way on Suse but not on Ubuntu or vice versa.
Other months it works the other way round.
defaultChunkSize = SigSt.chunkSize 256
-}


type Device = String

defaultDevice :: Device
defaultDevice = "default"


{- |
Useful values for the output device are

* @\"default\"@ for mixing with the output of other applications.

* @\"plughw:0,0\"@ for accessing sound output in an exclusive way.

* @\"tee:default,'output.raw',raw\"@ for playing and simultaneously writing raw data to disk.

* @\"tee:default,'output.wav',wav\"@ for playing and writing to WAVE file format.
  Note that the length cannot be written,
  when the program is terminated,
  leaving the file in an invalid format.
-}
makeSink ::
   (ALSA.SampleFmt y, RealRing.C t) =>
   Device {- ^ ALSA output device -} ->
   t {- ^ period (buffer) size expressed in seconds -} ->
   ALSA.SampleFreq {- ^ sample rate -} ->
   ALSA.SoundSink ALSA.Pcm y
makeSink device periodTime rate =
   ALSA.alsaSoundSinkTime device
      (ALSA.SoundFmt {
         ALSA.sampleFreq = rate
      }) $
   ALSA.SoundBufferTime
      (round (5000000*periodTime))
      (round (1000000*periodTime))

{-
alsaOpen: only few buffer underruns with
       let buffer_time = 200000 -- 0.20s
           period_time =  40000 -- 0.04s

However the delay is still perceivable.

Latency for keyboard playback might be better with:
       let buffer_time =  50000 -- 0.05s
           period_time =  10000 -- 0.01s
but we get too much underruns,
without actually achieving the required latency.
-}
{-# INLINE auto #-}
auto ::
   (ALSA.SampleFmt y) =>
   ALSA.SoundSink handle y ->
   SigSt.T y -> IO ()
auto sink ys =
   ALSA.withSoundSink sink $ \to ->
   writeLazy sink to ys

{-# INLINE writeLazy #-}
writeLazy ::
   (Storable y) =>
   ALSA.SoundSink handle y -> handle y ->
   SVL.Vector y -> IO ()
writeLazy sink to ys =
   mapM_ (write sink to) (SVL.chunks ys)

{-# INLINE write #-}
write ::
   (Storable y) =>
   ALSA.SoundSink handle y -> handle y ->
   SVB.Vector y -> IO ()
write sink to c =
   SVB.withStartPtr c $ \ptr size ->
   ALSA.soundSinkWrite sink to ptr size


-- cf. Alsa.hs
{-# INLINE arraySize #-}
arraySize :: Storable y => Ptr y -> Int -> Int
arraySize p n = advancePtr p n `minusPtr` p

{- |
Play a signal and write it to disk via SoX simultaneously.
Consider using 'auto' with @tee@ device.
-}
{-# INLINE autoAndRecord #-}
autoAndRecord ::
   (ALSA.SampleFmt y, SoxFrame.C y) =>
   FilePath ->
   ALSA.SoundFmt y ->
   ALSA.SoundSink handle y ->
   SigSt.T y -> IO Exit.ExitCode
autoAndRecord fileName fmt sink =
   let rate = ALSA.sampleFreq fmt
   in  (\act ->
          SoxWrite.simple act SoxOption.none fileName rate) $ \h ys ->
       ALSA.withSoundSink sink $ \to ->
       flip mapM_ (SVL.chunks ys) $ \c ->
       SVB.withStartPtr c $ \ptr size ->
       ALSA.soundSinkWrite sink to ptr size >>
       IO.hPutBuf h ptr (arraySize ptr size)


{- |
Play a signal and write it to multiple files.
The Functor @f@ may be @Maybe@ for no or one file to write,
or @[]@ for many files to write.
-}
{-# INLINE autoAndRecordMany #-}
autoAndRecordMany ::
   (ALSA.SampleFmt y, SoxFrame.C y,
    Trav.Traversable f) =>
   f FilePath ->
   ALSA.SoundFmt y ->
   ALSA.SoundSink handle y ->
   SigSt.T y -> IO (f Exit.ExitCode)
autoAndRecordMany fileNames fmt sink =
   let rate = ALSA.sampleFreq fmt
   in  (\act ->
          SoxWrite.manyExtended act SoxOption.none SoxOption.none fileNames rate) $ \hs ys ->
       ALSA.withSoundSink sink $ \to ->
       flip mapM_ (SVL.chunks ys) $ \c ->
       SVB.withStartPtr c $ \ptr size ->
       ALSA.soundSinkWrite sink to ptr size >>
       Fold.traverse_ (\h -> IO.hPutBuf h ptr (arraySize ptr size)) hs


{-# INLINE monoToInt16 #-}
monoToInt16 ::
   (Storable y, RealRing.C y) =>
   ALSA.SoundSink handle Int16 ->
   SigSt.T y -> IO ()
monoToInt16 sink xs =
   auto sink (SigSt.map BinSmp.int16FromCanonical xs)

{-# INLINE stereoToInt16 #-}
stereoToInt16 ::
   (Storable y, RealRing.C y) =>
   ALSA.SoundSink handle (Stereo.T Int16) ->
   SigSt.T (Stereo.T y) -> IO ()
stereoToInt16 sink xs =
   auto sink (SigSt.map (fmap BinSmp.int16FromCanonical) xs)
