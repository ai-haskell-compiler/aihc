{- |
Loading MIDI Files

This module loads and parses a MIDI File.
It can convert it into a 'MIDIFile.T' data type object or
simply print out the contents of the file.
-}

{-
The MIDI file format is quite similar to the Interchange File Format (IFF)
of Electronic Arts.
But it seems to be not sensible
to re-use functionality from the @iff@ package.
-}
module Sound.MIDI.File.Load (
   fromFile, fromByteList, maybeFromByteList, maybeFromByteString,
   showFile,
   ) where

import           Sound.MIDI.File
import qualified Sound.MIDI.File as MIDIFile
import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.File.Event as Event

import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Numeric.NonNegative.Wrapper as NonNeg

import Sound.MIDI.IO (ByteList, readBinaryFile, )
import           Sound.MIDI.String (unlinesS)
import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser
import qualified Sound.MIDI.Parser.Restricted as RestrictedParser
import qualified Sound.MIDI.Parser.ByteString as ByteStringParser
import qualified Sound.MIDI.Parser.Stream as StreamParser
import qualified Sound.MIDI.Parser.File   as FileParser
import qualified Sound.MIDI.Parser.Status as StatusParser
import qualified Sound.MIDI.Parser.Report as Report
import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM, liftM2, )

import qualified Data.ByteString.Lazy as B

import qualified Control.Monad.Exception.Asynchronous as Async
import Data.List (genericReplicate, genericLength, )
import Data.Maybe (catMaybes, )


{- |
The main load function.
Warnings are written to standard error output
and an error is signaled by a user exception.
This function will not be appropriate in GUI applications.
For these, use 'maybeFromByteString' instead.
-}
fromFile :: FilePath -> IO MIDIFile.T
fromFile =
   FileParser.runIncompleteFile parse


{-
fromFile :: FilePath -> IO MIDIFile.T
fromFile filename =
   do report <- fmap maybeFromByteList $ readBinaryFile filename
      mapM_ (hPutStrLn stderr . ("MIDI.File.Load warning: " ++)) (StreamParser.warnings report)
      either
         (ioError . userError . ("MIDI.File.Load error: " ++))
         return
         (StreamParser.result report)
-}

{- |
This function ignores warnings, turns exceptions into errors,
and return partial results without warnings.
Use this only in testing but never in production code!
-}
fromByteList :: ByteList -> MIDIFile.T
fromByteList contents =
   either
      error id
      (Report.result (maybeFromByteList contents))

maybeFromByteList ::
   ByteList -> Report.T MIDIFile.T
maybeFromByteList =
   StreamParser.runIncomplete parse . StreamParser.ByteList

maybeFromByteString ::
   B.ByteString -> Report.T MIDIFile.T
maybeFromByteString =
   ByteStringParser.runIncomplete parse



{- |
A MIDI file is made of /chunks/, each of which is either a /header chunk/
or a /track chunk/.  To be correct, it must consist of one header chunk
followed by any number of track chunks, but for robustness's sake we ignore
any non-header chunks that come before a header chunk.  The header tells us
the number of tracks to come, which is passed to 'getTracks'.
-}
parse :: Parser.C parser => Parser.Partial (Parser.Fragile parser) MIDIFile.T
parse =
   getChunk >>= \ (typ, hdLen) ->
      case typ of
        "MThd" ->
           do (format, nTracks, division) <-
                 RestrictedParser.runFragile hdLen getHeader
              excTracks <-
                 lift $ Parser.zeroOrMoreInc
                    (getTrackChunk >>= Async.mapM (lift . liftMaybe removeEndOfTrack))
              flip Async.mapM excTracks $ \tracks ->
                 do let n = genericLength tracks
                    lift $ Parser.warnIf (n /= nTracks)
                       ("header says " ++ show nTracks ++
                        " tracks, but " ++ show n ++ " tracks were found")
                    return (MIDIFile.Cons format division $ catMaybes tracks)
        _ -> lift (Parser.warn ("found Alien chunk <" ++ typ ++ ">")) >>
             Parser.skip hdLen >>
             parse


liftMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
liftMaybe f = maybe (return Nothing) (liftM Just . f)

{- |
There are two ways to mark the end of the track:
The end of the event list and the meta event 'EndOfTrack'.
Thus the end marker is redundant and we remove a 'EndOfTrack'
at the end of the track
and complain about all 'EndOfTrack's within the event list.
-}
removeEndOfTrack :: Parser.C parser => Track -> parser Track
removeEndOfTrack xs =
   maybe
      (Parser.warn "Empty track, missing EndOfTrack" >>
       return xs)
      (\(initEvents, lastEvent) ->
          let (eots, track) =
                 EventList.partition isEndOfTrack initEvents
          in  do Parser.warnIf
                    (not $ EventList.null eots)
                    "EndOfTrack inside a track"
                 Parser.warnIf
                    (not $ isEndOfTrack $ snd lastEvent)
                    "Track does not end with EndOfTrack"
                 return track)
      (EventList.viewR xs)

isEndOfTrack :: Event.T -> Bool
isEndOfTrack ev =
   case ev of
      Event.MetaEvent MetaEvent.EndOfTrack -> True
      _ -> False

{-
removeEndOfTrack :: Track -> Track
removeEndOfTrack =
   maybe
      (error "Track does not end with EndOfTrack")
      (\(ev,evs) ->
          case snd ev of
             MetaEvent EndOfTrack ->
                if EventList.null evs
                  then evs
                  else error "EndOfTrack inside a track"
             _ -> uncurry EventList.cons ev (removeEndOfTrack evs)) .
      EventList.viewL
-}

{- |
Parse a chunk, whether a header chunk, a track chunk, or otherwise.
A chunk consists of a four-byte type code
(a header is @MThd@; a track is @MTrk@),
four bytes for the size of the coming data,
and the data itself.
-}
getChunk :: Parser.C parser => Parser.Fragile parser (String, NonNeg.Integer)
getChunk =
   liftM2 (,)
      (getString 4)  -- chunk type: header or track
      (getNByteCardinal 4)
                     -- chunk body

getTrackChunk :: Parser.C parser => Parser.Partial (Parser.Fragile parser) (Maybe Track)
getTrackChunk =
   do (typ, len) <- getChunk
      if typ=="MTrk"
        then liftM (fmap Just) $ lift $
             RestrictedParser.run len $
             StatusParser.run getTrack
        else lift (Parser.warn ("found Alien chunk <" ++ typ ++ "> in track section")) >>
             Parser.skip len >>
             return (Async.pure Nothing)



{- |
Parse a Header Chunk.  A header consists of a format (0, 1, or 2),
the number of track chunks to come, and the smallest time division
to be used in reading the rest of the file.
-}
getHeader :: Parser.C parser => Parser.Fragile parser (MIDIFile.Type, NonNeg.Int, Division)
getHeader =
   do
      format   <- makeEnum =<< get2
      nTracks  <- liftM (NonNeg.fromNumberMsg "MIDI.Load.getHeader") get2
      division <- getDivision
      return (format, nTracks, division)

{- |
The division is implemented thus: the most significant bit is 0 if it's
in ticks per quarter note; 1 if it's an SMPTE value.
-}
getDivision :: Parser.C parser => Parser.Fragile parser Division
getDivision =
   do
      x <- get1
      y <- get1
      return $
         if x < 128
           then Ticks (NonNeg.fromNumberMsg "MIDI.Load.getDivision" (x*256+y))
           else SMPTE (256-x) y

{- |
A track is a series of events.  Parse a track, stopping when the size
is zero.
-}
getTrack :: Parser.C parser => Parser.Partial (StatusParser.T parser) MIDIFile.Track
getTrack =
   liftM
      (fmap EventList.fromPairList)
      (Parser.zeroOrMore Event.getTrackEvent)



-- * show contents of a MIDI file for debugging

{-# DEPRECATED showFile "only use this for debugging" #-}
{- |
Functions to show the decoded contents of a MIDI file in an easy-to-read format.
This is for debugging purposes and should not be used in production code.
-}
showFile :: FilePath -> IO ()
showFile fileName = putStr . showChunks =<< readBinaryFile fileName

showChunks :: ByteList -> String
showChunks mf =
  showMR (lift getChunks) (\(Async.Exceptional me cs) ->
     unlinesS (map pp cs) .
     maybe id (\e -> showString ("incomplete chunk list: " ++ e ++ "\n")) me) mf ""
 where
  pp :: (String, ByteList) -> ShowS
  pp ("MThd",contents) =
    showString "Header: " .
    showMR getHeader shows contents
  pp ("MTrk",contents) =
    showString "Track:\n" .
    showMR (lift $ StatusParser.run getTrack)
        (\(Async.Exceptional me track) str ->
            EventList.foldr
               MIDIFile.showTime
               (\e -> MIDIFile.showEvent e . showString "\n")
               (maybe "" (\e -> "incomplete track: " ++ e ++ "\n") me ++ str) track)
        contents
  pp (ty,contents) =
    showString "Alien Chunk: " .
    showString ty .
    showString " " .
    shows contents .
    showString "\n"


showMR :: Parser.Fragile (StreamParser.T StreamParser.ByteList) a -> (a->ShowS) -> ByteList -> ShowS
showMR m pp contents =
  let report = StreamParser.run m (StreamParser.ByteList contents)
  in  unlinesS (map showString $ Report.warnings report) .
      either showString pp (Report.result report)



{- |
The two functions, the 'getChunk' and 'getChunks' parsers,
do not combine directly into a single master parser.
Rather, they should be used to chop parts of a midi file
up into chunks of bytes which can be outputted separately.

Chop a MIDI file into chunks returning:

* list of /chunk-type/-contents pairs; and
* leftover slop (should be empty in correctly formatted file)

-}
getChunks ::
   Parser.C parser => Parser.Partial parser [(String, ByteList)]
getChunks =
   Parser.zeroOrMore $
      do (typ, len) <- getChunk
         body <- sequence (genericReplicate len getByte)
         return (typ, body)
