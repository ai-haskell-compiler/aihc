{- |
System messages
-}
module Sound.MIDI.Message.System (
   T(..), get, getIncomplete, put,
   ) where

import qualified Sound.MIDI.Message.System.Exclusive as Exclusive
import qualified Sound.MIDI.Message.System.Common    as Common
import qualified Sound.MIDI.Message.System.RealTime  as RealTime

import qualified Sound.MIDI.Parser.Class as Parser

import qualified Sound.MIDI.Writer.Basic as Writer

import qualified Control.Monad.Exception.Asynchronous as Async

import Control.Monad (liftM, )



data T =
     Exclusive Exclusive.T
   | Common    Common.T
   | RealTime  RealTime.T


get :: Parser.C parser => Int -> Parser.Fragile parser T
get code =
   if code == 0xF0
     then liftM Exclusive Exclusive.get
     else
       if code >= 0xF1 && code <= 0xF6
         then liftM Common $ Common.get code
         else
           if code >= 0xF8 && code <= 0xFF
             then liftM RealTime $ RealTime.get code
             else Parser.giveUp ("invalid System message code " ++ show code)

getIncomplete :: Parser.C parser => Int -> Parser.Partial (Parser.Fragile parser) T
getIncomplete code =
   if code == 0xF0
     then liftM (fmap Exclusive) Exclusive.getIncomplete
     else
       if code >= 0xF1 && code <= 0xF6
         then liftM (Async.pure . Common) $ Common.get code
         else
           if code >= 0xF8 && code <= 0xFF
             then liftM (Async.pure . RealTime) $ RealTime.get code
             else Parser.giveUp ("invalid System message code " ++ show code)


put :: Writer.C writer => T -> writer
put msg =
   case msg of
      Exclusive s -> Exclusive.put s
      Common s    -> Common.put s
      RealTime s  -> RealTime.put s
