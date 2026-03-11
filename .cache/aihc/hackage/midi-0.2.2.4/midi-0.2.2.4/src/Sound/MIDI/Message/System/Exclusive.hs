{- |
System Exclusive messages
-}
module Sound.MIDI.Message.System.Exclusive (
   T(..), get, getIncomplete, put,
   ) where

import qualified Sound.MIDI.Manufacturer as Manufacturer
import Sound.MIDI.IO (ByteList)

import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser

import qualified Sound.MIDI.Writer.Basic as Writer
import Sound.MIDI.Monoid ((+#+))

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Exception.Asynchronous as Async

import Data.Maybe (fromMaybe, )


data T =
     Commercial    Manufacturer.T ByteList
   | NonCommercial ByteList
   | NonRealTime   NonRealTime
   | RealTime      RealTime


-- * Non-real time

{-# DEPRECATED NonRealTime "structure must be defined, yet" #-}
newtype NonRealTime = NonRealTimeCons ByteList

-- * Real time

{-# DEPRECATED RealTime "structure must be defined, yet" #-}
newtype RealTime = RealTimeCons ByteList


-- * serialization

get :: Parser.C parser => Parser.Fragile parser T
get =
   do (Async.Exceptional err sysex) <- getIncomplete
      maybe (return sysex) Parser.giveUp err

getIncomplete :: Parser.C parser => Parser.Partial (Parser.Fragile parser) T
getIncomplete =
   do manu <- Manufacturer.get
      incBody <- MT.lift getBody
      return $ flip fmap incBody $ \body ->
         fromMaybe (Commercial manu body) $
         lookup manu $
            (Manufacturer.nonCommercial, NonCommercial body) :
            (Manufacturer.nonRealTime,   NonRealTime $ NonRealTimeCons body) :
            (Manufacturer.realTime,      RealTime    $ RealTimeCons body) :
            []

getBody :: Parser.C parser => Parser.Partial parser ByteList
getBody = Parser.until (0xf7 ==) getByte


{- |
It is not checked whether SysEx messages contain only 7-bit values.
-}
put :: Writer.C writer => T -> writer
put sysex =
   case sysex of
      Commercial manu body ->
         Manufacturer.put manu +#+
         Writer.putByteList body
      NonCommercial body ->
         Manufacturer.put Manufacturer.nonCommercial +#+
         Writer.putByteList body
      NonRealTime (NonRealTimeCons body) ->
         Manufacturer.put Manufacturer.nonRealTime +#+
         Writer.putByteList body
      RealTime (RealTimeCons body) ->
         Manufacturer.put Manufacturer.realTime +#+
         Writer.putByteList body
