{- |
Save MIDI data to files.

The functions in this module allow 'Sound.MIDI.File.T's
to be written into Standard MIDI files (@*.mid@)
that can be read and played by music programs such as Cakewalk.
-}

module Sound.MIDI.File.Save
   (toSeekableFile, toFile, toByteList, toByteString,
    toCompressedByteString, ) where

import           Sound.MIDI.File
import qualified Sound.MIDI.File   as MIDIFile
import qualified Sound.MIDI.File.Event      as Event
import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Sound.MIDI.Writer.Status as StatusWriter
import qualified Sound.MIDI.Writer.Basic  as Writer
import Sound.MIDI.Monoid ((+#+))

import qualified Data.Monoid.Transformer as Trans
import Data.Foldable (foldMap, )

import Sound.MIDI.IO (ByteList, writeBinaryFile, )

import qualified Data.ByteString.Lazy as B



{- |
Directly write to a file.
Since chunks lengths are not known before writing,
we need to seek in a file.
Thus you cannot write to pipes with this function.
-}
toSeekableFile :: FilePath {- ^ file name -} -> MIDIFile.T -> IO ()
toSeekableFile fn =
   Writer.runSeekableFile fn . StatusWriter.toWriterWithoutStatus . put

{- |
The function 'toFile' is the main function
for writing 'MIDIFile.T' values to an actual file.
-}
toFile :: FilePath {- ^ file name -} -> MIDIFile.T -> IO ()
toFile fn mf = writeBinaryFile fn (toByteList mf)

{-
MIDI files are first converted to a monadic string computation
using the function 'put',
and then \"executed\" using 'execWriter'.
-}

{- |
Convert a MIDI file to a 'ByteList'.
-}
toByteList :: MIDIFile.T -> ByteList
toByteList =
   Writer.runByteList . StatusWriter.toWriterWithoutStatus . put

{- |
Convert a MIDI file to a lazy 'B.ByteString'.
-}
toByteString :: MIDIFile.T -> B.ByteString
toByteString =
   Writer.runByteString . StatusWriter.toWriterWithoutStatus . put

{- |
Convert a MIDI file to a lazy 'B.ByteString'.
It converts @NoteOff p 64@ to @NoteOn p 0@
and then uses the running MIDI status in order to compress the file.
-}
toCompressedByteString :: MIDIFile.T -> B.ByteString
toCompressedByteString =
   Writer.runByteString . StatusWriter.toWriterWithStatus . put .
   MIDIFile.implicitNoteOff




put ::
   (StatusWriter.Compression compress, Writer.C writer) =>
   MIDIFile.T -> StatusWriter.T compress writer
put (MIDIFile.Cons mft divisn trks) =
   (putChunk "MThd" $ StatusWriter.lift $
      Writer.putInt 2 (fromEnum mft) +#+ -- format (type 0, 1 or 2)
      Writer.putInt 2 (length trks)  +#+ -- number of tracks to come
      putDivision divisn)                 -- time unit
   +#+ foldMap putTrack trks

putDivision :: Writer.C writer => Division -> writer
putDivision (Ticks nticks) =
   Writer.putInt 2 (NonNeg.toNumber nticks)
putDivision (SMPTE mode nticks) =
   Writer.putIntAsByte (256-mode) +#+
   Writer.putIntAsByte nticks

putTrack ::
   (StatusWriter.Compression compress, Writer.C writer) =>
   Track -> StatusWriter.T compress writer
putTrack trk =
   putChunk "MTrk" $
   EventList.concatMapMonoid (StatusWriter.lift . Writer.putVar) Event.put $
   EventList.snoc trk 0 (Event.MetaEvent MetaEvent.EndOfTrack)



putChunk ::
   (StatusWriter.Compression compress, Writer.C writer) =>
   String -> StatusWriter.T compress writer -> StatusWriter.T compress writer
putChunk tag m =
   StatusWriter.lift (putTag tag) +#+
   (StatusWriter.Cons $ Trans.lift $
    Writer.putLengthBlock 4 $ StatusWriter.toWriter m)


putTag :: Writer.C writer => String -> writer
putTag tag@(_:_:_:_:[]) = Writer.putStr tag
putTag tag =
   error ("SaveMIDI.putChunk: Chunk name " ++ tag ++
          " does not consist of 4 characters.")
