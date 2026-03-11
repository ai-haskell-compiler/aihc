{- |
MIDI messages for real-time communication with MIDI devices.
This does not cover MIDI file events.
For these refer to "Sound.MIDI.File.Event".
-}
module Sound.MIDI.Message (
   T(..),
   get, getWithStatus, getIncompleteWithStatus,
   put, putWithStatus,
   maybeFromByteString, toByteString,
   ) where

import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.System  as System

import qualified Sound.MIDI.Parser.Status as StatusParser
import qualified Sound.MIDI.Parser.Class  as Parser
import           Sound.MIDI.Parser.Primitive (get1)
import qualified Sound.MIDI.Parser.ByteString as ParserByteString

import qualified Sound.MIDI.Writer.Status as StatusWriter
import qualified Sound.MIDI.Writer.Basic  as Writer
import Sound.MIDI.Monoid ((+#+))

import qualified Sound.MIDI.Parser.Report as Report

import qualified Control.Monad.Exception.Asynchronous as Async

import Control.Monad (liftM, )

import qualified Data.ByteString.Lazy as B


data T =
     Channel Channel.T
   | System  System.T
-- Show instance requires Show instance of System.T
--     deriving (Show)


get :: Parser.C parser => Parser.Fragile parser T
get =
   get1 >>= \code ->
   if code >= 0xF0
     then liftM System  $ System.get code
     else liftM Channel $ (uncurry Channel.get (Channel.decodeStatus code) =<< get1)
--     else liftM Channel $ StatusParser.run (Channel.getWithStatus code)

getWithStatus :: Parser.C parser => Parser.Fragile (StatusParser.T parser) T
getWithStatus =
   StatusParser.lift get1 >>= \code ->
   if code >= 0xF0
     then StatusParser.set Nothing >>
          (liftM System $ StatusParser.lift $ System.get code)
     else liftM Channel $ Channel.getWithStatus code

getIncompleteWithStatus ::
   Parser.C parser => Parser.Partial (Parser.Fragile (StatusParser.T parser)) T
getIncompleteWithStatus =
   StatusParser.lift get1 >>= \code ->
   if code >= 0xF0
     then liftM (fmap System) $ StatusParser.lift $ System.getIncomplete code
     else liftM (Async.pure . Channel) $ Channel.getWithStatus code

maybeFromByteString :: B.ByteString -> Report.T T
maybeFromByteString =
   ParserByteString.run get




put :: Writer.C writer => T -> writer
put msg =
   case msg of
      Channel s -> Channel.put s
      System  s -> System.put  s

putWithStatus ::
   (StatusWriter.Compression compress, Writer.C writer) =>
   T -> StatusWriter.T compress writer
putWithStatus msg =
   case msg of
      Channel s -> Channel.putWithStatus s
      System  s -> StatusWriter.clear +#+ StatusWriter.lift (System.put s)

toByteString :: T -> B.ByteString
toByteString =
   Writer.runByteString . put
