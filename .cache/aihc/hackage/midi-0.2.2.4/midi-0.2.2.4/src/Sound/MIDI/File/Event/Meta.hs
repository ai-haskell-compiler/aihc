module Sound.MIDI.File.Event.Meta (
   T(..),
   ElapsedTime, fromElapsedTime, toElapsedTime,
   Tempo,       fromTempo,       toTempo,
   defltTempo,
   SMPTEHours, SMPTEMinutes, SMPTESeconds, SMPTEFrames, SMPTEBits,
   get, put, ) where

import Sound.MIDI.Message.Channel (Channel, toChannel, fromChannel, )

import qualified Sound.MIDI.KeySignature as KeySig

import Sound.MIDI.Parser.Primitive (get1, get2, get3, getVar, getBigN, )
import qualified Sound.MIDI.Parser.Class as Parser
import qualified Sound.MIDI.Parser.Restricted as ParserRestricted

import Control.Monad (liftM, liftM4, liftM5, )

import qualified Sound.MIDI.Writer.Basic as Writer
import qualified Sound.MIDI.Bit as Bit

import Sound.MIDI.Monoid ((+#+))

import qualified Numeric.NonNegative.Wrapper as NonNeg
import Sound.MIDI.IO (ByteList, listCharFromByte, listByteFromChar, )

import Sound.MIDI.Utility
         (arbitraryString, arbitraryByteList, )

import Test.QuickCheck (Arbitrary(arbitrary), )
import qualified Test.QuickCheck as QC

import Prelude hiding (putStr, )


{- * Meta Events -}

type ElapsedTime  = NonNeg.Integer
type Tempo        = NonNeg.Int
type SMPTEHours   = Int
type SMPTEMinutes = Int
type SMPTESeconds = Int
type SMPTEFrames  = Int
type SMPTEBits    = Int

data T =
     SequenceNum Int
   | TextEvent String
   | Copyright String
   | TrackName String
   | InstrumentName String
   | Lyric String
   | Marker String
   | CuePoint String
   | MIDIPrefix Channel
   | EndOfTrack
   | SetTempo Tempo
   | SMPTEOffset SMPTEHours SMPTEMinutes SMPTESeconds SMPTEFrames SMPTEBits
   | TimeSig Int Int Int Int
   | KeySig KeySig.T
   | SequencerSpecific ByteList
   | Unknown Int ByteList
     deriving (Show, Eq, Ord)


instance Arbitrary T where
   arbitrary =
      QC.oneof $
         liftM  SequenceNum (QC.choose (0,0xFFFF)) :
         liftM  TextEvent arbitraryString :
         liftM  Copyright arbitraryString :
         liftM  TrackName arbitraryString :
         liftM  InstrumentName arbitraryString :
         liftM  Lyric arbitraryString :
         liftM  Marker arbitraryString :
         liftM  CuePoint arbitraryString :
         liftM  (MIDIPrefix . toChannel) (QC.choose (0,15)) :
--         return EndOfTrack :
         liftM  (SetTempo . NonNeg.fromNumberMsg "Tempo always positive") (QC.choose (0,0xFFFFFF)) :
         liftM5 SMPTEOffset arbitraryByte arbitraryByte arbitraryByte arbitraryByte arbitraryByte :
         liftM4 TimeSig arbitraryByte arbitraryByte arbitraryByte arbitraryByte :
         liftM  KeySig arbitrary :
         liftM  SequencerSpecific arbitraryByteList :
--         liftM  Unknown arbitrary arbitraryByteList :
         []

arbitraryByte :: QC.Gen Int
arbitraryByte = QC.choose (0,0xFF::Int)


toElapsedTime :: Integer -> ElapsedTime
toElapsedTime = NonNeg.fromNumberMsg "toElapsedTime"

fromElapsedTime :: ElapsedTime -> Integer
fromElapsedTime = NonNeg.toNumber


toTempo :: Int -> Tempo
toTempo = NonNeg.fromNumberMsg "toTempo"

fromTempo :: Tempo -> Int
fromTempo = NonNeg.toNumber

{- |
The default SetTempo value, in microseconds per quarter note.
This expresses the default of 120 beats per minute.
-}
defltTempo :: Tempo
defltTempo = 500000



-- * serialization

get :: Parser.C parser => Parser.Fragile parser T
get =
   do code <- get1
      len  <- getVar
      let parse = ParserRestricted.runFragile len
      let returnText cons = liftM (cons . listCharFromByte) $ getBigN len
      case code of
         000 -> parse $ liftM SequenceNum get2
         001 -> returnText TextEvent
         002 -> returnText Copyright
         003 -> returnText TrackName
         004 -> returnText InstrumentName
         005 -> returnText Lyric
         006 -> returnText Marker
         007 -> returnText CuePoint

         032 -> parse $
                liftM (MIDIPrefix . toChannel) get1
         047 -> return EndOfTrack
         081 -> parse $
                liftM (SetTempo . toTempo) get3

         084 -> parse $
                do {hrs    <- get1 ; mins <- get1 ; secs <- get1;
                    frames <- get1 ; bits <- get1 ;
                    return (SMPTEOffset hrs mins secs frames bits)}

         088 -> parse $
                do
                   n <- get1
                   d <- get1
                   c <- get1
                   b <- get1
                   return (TimeSig n d c b)

         089 -> parse $ liftM KeySig KeySig.get

         127 -> liftM SequencerSpecific $ getBigN len

         _   -> liftM (Unknown code) $ getBigN len


put :: Writer.C writer => T -> writer
put ev =
   Writer.putByte 255 +#+
   case ev of
     SequenceNum num  -> putInt    0 2 num
     TextEvent s      -> putStr    1 s
     Copyright s      -> putStr    2 s
     TrackName s      -> putStr    3 s
     InstrumentName s -> putStr    4 s
     Lyric s          -> putStr    5 s
     Marker s         -> putStr    6 s
     CuePoint s       -> putStr    7 s
     MIDIPrefix c     -> putList  32 [fromChannel c]
     EndOfTrack       -> putList  47 []

     SetTempo tp      -> putInt   81 3 (fromTempo tp)
     SMPTEOffset hr mn se fr ff
                      -> putList  84 [hr,mn,se,fr,ff]
     TimeSig n d c b  -> putList  88 [n,d,c,b]
     KeySig key       -> putList  89 $ KeySig.toBytes key
     SequencerSpecific codes
                      -> putByteList 127 codes
     Unknown typ s    -> putByteList typ s


putByteList :: Writer.C writer => Int -> ByteList -> writer
putByteList code bytes =
   Writer.putIntAsByte code +#+
   Writer.putLenByteList bytes

putInt :: Writer.C writer => Int -> Int -> Int -> writer
putInt code numBytes x =
   Writer.putIntAsByte code +#+
   Writer.putVar (fromIntegral numBytes) +#+
   Writer.putByteList
      (map fromIntegral $ Bit.someBytes numBytes x)

putStr :: Writer.C writer => Int -> String -> writer
putStr code =
   putByteList code . listByteFromChar

putList :: Writer.C writer => Int -> [Int] -> writer
putList code =
   putByteList code . map fromIntegral
