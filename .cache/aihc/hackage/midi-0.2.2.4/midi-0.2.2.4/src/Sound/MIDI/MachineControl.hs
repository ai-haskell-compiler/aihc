module Sound.MIDI.MachineControl (
   splitCommandList,

   getCommand,
   getCommands,

   Command (
      Stop,
      Play,
      DeferredPlay,
      FastForward,
      Rewind,
      RecordStrobe,
      RecordExit,
      RecordPause,
      Pause,
      Eject,
      Chase,
      CommandErrorReset,
      Reset,
      Wait,
      Resume
      {-
      I will export more constructors, when I am sure,
      that their definition is reasonable.
      -}
      ),

   runParser,

   ) where

import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser
import qualified Sound.MIDI.Parser.Stream as SP

import Sound.MIDI.IO (ByteList, )

import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM, liftM2, liftM3, )

import Data.List (unfoldr, )
import Data.Tuple.HT (mapFst, )
import Data.Bool.HT (if', )
import Data.Word (Word8, )
import Data.Maybe (isNothing, catMaybes, )


-- * serialization

splitCommandList :: [Word8] -> [(Word8, [Word8])]
splitCommandList =
   unfoldr $ \xt ->
      case xt of
         [] -> Nothing
         x:xs ->
            Just $ (mapFst ((,) x)) $
            if' (x==0 || x==0xF7) (xs, []) $
            if' (0x40 <= x && x < 0x78)
                   (case xs of
                      [] -> ([], [])
                      n:ys -> splitAt (fromIntegral n) ys) $
            ([], xs)


data Command =
     Stop
   | Play
   | DeferredPlay
   | FastForward
   | Rewind
   | RecordStrobe
   | RecordExit
   | RecordPause
   | Pause
   | Eject
   | Chase
   | CommandErrorReset
   | Reset

   | Write ByteList
   | MaskedWrite ByteList
   | Read ByteList
   | Update ByteList
   | Locate ByteList
   | VariablePlay Word8 Word8 Word8
   | Search Word8 Word8 Word8
   | Shuttle Word8 Word8 Word8
   | Step Word8
   | AssignSystemMaster Word8
   | GeneratorCommand Word8
   | MIDITimeCodeCommand Word8
   | Move Word8 Word8
   | Add Word8 Word8 Word8
   | Subtract Word8 Word8 Word8
   | DropFrameAdjust Word8
   | Procedure ByteList
   | Event ByteList
   | Group ByteList
   | CommandSegment ByteList
   | DeferredVariablePlay ByteList
   | RecordStrobeVariable ByteList

   | Wait
   | Resume

   | GenericNoData Word8
   | GenericVariableLength Word8 ByteList
   deriving (Show)


{- |
Read MIDI machine control commands
until an F7 marker for SysEx end.
-}
getCommands :: Parser.C parser => Parser.Partial parser [Command]
getCommands =
   liftM (fmap catMaybes) $
   Parser.until isNothing $ do
      code <- getByte
      if code == 0xF7
        then return Nothing
        else liftM Just $ getCommand code

getCommand :: Parser.C parser => Word8 -> Parser.Fragile parser Command
getCommand code =
   let fetchMany f = liftM f $ getN . NonNeg.fromNumberMsg "Midi.get1" =<< get1
       fetchN reqLen act = do
          len <- get1
          if len==reqLen
            then act
            else Parser.giveUp $
                 "expect " ++ show reqLen ++
                 " argument(s) for command " ++ show code ++
                 ", but got " ++ show len
       fetch1 f = fetchN 1 (liftM  f getByte)
       fetch2 f = fetchN 2 (liftM2 f getByte getByte)
       fetch3 f = fetchN 3 (liftM3 f getByte getByte getByte)

   in  case code of
          0x01 -> return Stop
          0x02 -> return Play
          0x03 -> return DeferredPlay
          0x04 -> return FastForward
          0x05 -> return Rewind
          0x06 -> return RecordStrobe
          0x07 -> return RecordExit
          0x08 -> return RecordPause
          0x09 -> return Pause
          0x0A -> return Eject
          0x0B -> return Chase
          0x0C -> return CommandErrorReset
          0x0D -> return Reset

          0x40 -> fetchMany Write
          0x41 -> fetchMany MaskedWrite
          0x42 -> fetchMany Read
          0x43 -> fetchMany Update
          0x44 -> fetchMany Locate
          0x45 -> fetch3 VariablePlay
          0x46 -> fetch3 Search
          0x47 -> fetch3 Shuttle
          0x48 -> fetch1 Step
          0x49 -> fetch1 AssignSystemMaster
          0x4A -> fetch1 GeneratorCommand
          0x4B -> fetch1 MIDITimeCodeCommand
          0x4C -> fetch2 Move
          0x4D -> fetch3 Add
          0x4E -> fetch3 Subtract
          0x4F -> fetch1 DropFrameAdjust
          0x50 -> fetchMany Procedure
          0x51 -> fetchMany Event
          0x52 -> fetchMany Group
          0x53 -> fetchMany CommandSegment
          0x54 -> fetchMany DeferredVariablePlay
          0x55 -> fetchMany RecordStrobeVariable

          0x7C -> return Wait
          0x7F -> return Resume

          0x00 -> Parser.giveUp "encountered command zero"
          0xF7 -> Parser.giveUp "end of SysEx" -- should be handled by the caller

          _ ->
             if' (0x40 <= code && code < 0x78)
                (fetchMany $ GenericVariableLength code)
                (return $ GenericNoData code)

runParser ::
   Parser.Partial (SP.T SP.ByteList) a ->
   ByteList ->
   (SP.PossiblyIncomplete a, [SP.UserMessage])
runParser p =
   MS.evalState (MW.runWriterT $ SP.decons p) .
   SP.ByteList
