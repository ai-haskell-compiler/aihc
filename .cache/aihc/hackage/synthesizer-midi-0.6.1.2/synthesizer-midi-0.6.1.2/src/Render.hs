{- |
Very simple MIDI file renderer.
It uses an arbitrary set of simple instruments,
that is in no way related to General MIDI or something else,
ignores tempo changes and respects only MIDI channel 0.
-}
module Main where

import qualified Synthesizer.MIDI.Example.Instrument as Instr
import qualified Synthesizer.MIDI.Storable as MidiSt

import qualified Synthesizer.Frame.Stereo as Stereo

import qualified Data.StorableVector.Lazy as SVL

import qualified Sound.Sox.Write as SoxWrite
import qualified Sound.Sox.Play  as SoxPlay

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Event as FileEvent
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import Sound.MIDI.Message.Channel (Channel, )

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Numeric.NonNegative.Wrapper as NonNeg

import Control.Monad.Trans.State (evalState, )

import Data.Monoid (mempty, )

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO

import NumericPrelude.Numeric ((*>), )
import NumericPrelude.Base
import Prelude (Num, Float, fromInteger, )


type Real = Float

channel :: Channel
channel = ChannelMsg.toChannel 0

sampleRate :: Num a => a
-- sampleRate = 24000
-- sampleRate = 48000
sampleRate = 44100

chunkSize :: SVL.ChunkSize
chunkSize = SVL.defaultChunkSize


render ::
   MidiFile.T -> SVL.Vector (Stereo.T Real)
render =
   SVL.map ((0.2::Real)*>) .
   evalState
      (MidiSt.sequenceMultiProgram chunkSize channel
         (VoiceMsg.toProgram 0) $
            Instr.pingStereoRelease :
            Instr.tineStereo :
            Instr.softString :
            []) .
   EventList.collectCoincident .
   EventList.mapMaybe (\ev ->
      case ev of
         FileEvent.MIDIEvent mev -> Just mev
         _ -> Nothing) .
   EventList.mapTime
      (NonNeg.fromNumberMsg "MIDI.render" . fromInteger . NonNeg.toNumber) .
   EventList.resample sampleRate .
   (\(MidiFile.Cons typ division tracks) ->
      MidiFile.mergeTracks typ $
      map (MidiFile.secondsFromTicks division) tracks)

handleSoxExit :: IO Exit.ExitCode -> IO ()
handleSoxExit sox = do
   soxResult <- sox
   case soxResult of
      Exit.ExitSuccess -> return ()
      Exit.ExitFailure n -> do
         IO.hPutStrLn IO.stderr $
            "'sox' aborted with exit code " ++ show n
         Exit.exitFailure

main :: IO ()
main = do
   args <- Env.getArgs
   case args of
      [midiPath] ->
         handleSoxExit .
            SoxPlay.simple SVL.hPut mempty sampleRate .
            render =<<
            Load.fromFile midiPath
      [midiPath, wavePath] ->
         handleSoxExit .
            SoxWrite.simple SVL.hPut mempty wavePath sampleRate .
            render =<<
            Load.fromFile midiPath
      _ -> do
         IO.hPutStrLn IO.stderr
            "need arguments: infile.mid [outfile.wav]"
         Exit.exitFailure
