{- |
Channel voice messages
-}
module Sound.MIDI.Message.Channel.Voice (
   T(..), get, putWithStatus,
   ControllerValue, PitchBendRange, Pressure,
   isNote, isNoteOn, isNoteOff, zeroKey,
   explicitNoteOff, implicitNoteOff,
   realFromControllerValue,

   bankSelect, modulation, breathControl, footControl, portamentoTime,
   dataEntry, mainVolume, balance, panorama, expression,
   generalPurpose1, generalPurpose2, generalPurpose3, generalPurpose4,
   vectorX, vectorY,

   bankSelectMSB, modulationMSB, breathControlMSB, footControlMSB,
   portamentoTimeMSB, dataEntryMSB, mainVolumeMSB, balanceMSB,
   panoramaMSB, expressionMSB, generalPurpose1MSB, generalPurpose2MSB,
   generalPurpose3MSB, generalPurpose4MSB, bankSelectLSB, modulationLSB,
   breathControlLSB, footControlLSB, portamentoTimeLSB, dataEntryLSB,
   mainVolumeLSB, balanceLSB, panoramaLSB, expressionLSB,
   generalPurpose1LSB, generalPurpose2LSB, generalPurpose3LSB, generalPurpose4LSB,

   sustain, porta, sustenuto, softPedal, hold2,
   generalPurpose5, generalPurpose6, generalPurpose7, generalPurpose8,
   extDepth, tremoloDepth, chorusDepth, celesteDepth, phaserDepth,

   dataIncrement, dataDecrement,
   nonRegisteredParameterLSB, nonRegisteredParameterMSB,
   registeredParameterLSB, registeredParameterMSB,

   Pitch,      fromPitch,      toPitch,
   Velocity,   fromVelocity,   toVelocity,
   Program,    fromProgram,    toProgram,
   CtrlP.Controller, CtrlP.fromController, CtrlP.toController,

   increasePitch, subtractPitch, frequencyFromPitch,
   maximumVelocity, normalVelocity, realFromVelocity,
  ) where

import qualified Sound.MIDI.ControllerPrivate as CtrlP
import qualified Sound.MIDI.Controller as Ctrl

import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser

import Control.Monad (liftM, liftM2, )

import qualified Sound.MIDI.Writer.Status as StatusWriter
import qualified Sound.MIDI.Writer.Basic  as Writer
import qualified Sound.MIDI.Bit as Bit

import Sound.MIDI.Monoid ((+#+))

import Data.Ix (Ix)
import Sound.MIDI.Utility (checkRange,
          quantityRandomR, boundedQuantityRandom, chooseQuantity,
          enumRandomR, boundedEnumRandom, chooseEnum, )

import Test.QuickCheck (Arbitrary(arbitrary), )
import qualified Test.QuickCheck as QC
import System.Random (Random(random, randomR), )



-- * message type

data T =
     NoteOff        Pitch Velocity
   | NoteOn         Pitch Velocity
   | PolyAftertouch Pitch Pressure
   | ProgramChange  Program
   {-
   Shall we add support for registered parameters?
   -}
   | Control        Ctrl.T ControllerValue
   | PitchBend      PitchBendRange
   | MonoAftertouch Pressure
     deriving (Show, Eq, Ord)


instance Arbitrary T where
   arbitrary =
      QC.frequency $
         (10, liftM2 NoteOff        arbitrary arbitrary) :
         (10, liftM2 NoteOn         arbitrary arbitrary) :
         ( 1, liftM2 PolyAftertouch arbitrary (QC.choose (0,127))) :
         ( 1, liftM  ProgramChange  arbitrary) :
         ( 1, liftM2 Control        arbitrary (QC.choose (0,127))) :
         ( 1, liftM  PitchBend      (QC.choose (0,12))) :
         ( 1, liftM  MonoAftertouch (QC.choose (0,127))) :
         []


instance Random Pitch where
   random  = boundedEnumRandom
   randomR = enumRandomR

instance Arbitrary Pitch where
   arbitrary = chooseEnum


instance Random Velocity where
   random  = boundedQuantityRandom fromVelocity toVelocity
   randomR = quantityRandomR fromVelocity toVelocity

instance Arbitrary Velocity where
   arbitrary = chooseQuantity fromVelocity toVelocity


instance Random Program where
   random  = boundedEnumRandom
   randomR = enumRandomR

instance Arbitrary Program where
   arbitrary = chooseEnum


isNote :: T -> Bool
isNote (NoteOn  _ _) = True
isNote (NoteOff _ _) = True
isNote _             = False

{- |
NoteOn with zero velocity is considered NoteOff according to MIDI specification.
-}
isNoteOn :: T -> Bool
isNoteOn (NoteOn  _ v) = v > toVelocity 0
isNoteOn _             = False

{- |
NoteOn with zero velocity is considered NoteOff according to MIDI specification.
-}
isNoteOff :: T -> Bool
isNoteOff (NoteOn  _ v) = v == toVelocity 0
isNoteOff (NoteOff _ _) = True
isNoteOff _             = False


{- |
Convert all @NoteOn p 0@ to @NoteOff p 64@.
The latter one is easier to process.
-}
explicitNoteOff :: T -> T
explicitNoteOff msg =
   case msg of
      NoteOn p v ->
         if v == toVelocity 0
           then NoteOff p $ toVelocity 64
           else msg
      _ -> msg


{- |
Convert all @NoteOff p 64@ to @NoteOn p 0@.
The latter one can be encoded more efficiently using the running status.
-}
implicitNoteOff :: T -> T
implicitNoteOff msg =
   case msg of
      NoteOff p v ->
         if v == toVelocity 64
           then NoteOn p $ toVelocity 0
           else msg
      _ -> msg




-- * Primitive types in Voice messages

type PitchBendRange  = Int
type Pressure        = Int
type ControllerValue = Ctrl.Value


newtype Pitch      = Pitch      {fromPitch      :: Int} deriving (Show, Eq, Ord, Ix)
newtype Velocity   = Velocity   {fromVelocity   :: Int} deriving (Show, Eq, Ord)
newtype Program    = Program    {fromProgram    :: Int} deriving (Show, Eq, Ord, Ix)


toPitch :: Int -> Pitch
toPitch = checkRange "Pitch" Pitch

toVelocity :: Int -> Velocity
toVelocity = checkRange "Velocity" Velocity

toProgram :: Int -> Program
toProgram = checkRange "Program" Program


instance Enum Pitch where
   toEnum   = toPitch
   fromEnum = fromPitch

{-
I do not like an Enum Velocity instance,
because Velocity is an artificially sampled continuous quantity.
-}

instance Enum Program where
   toEnum   = toProgram
   fromEnum = fromProgram

-- typical methods of a type class for affine spaces
increasePitch :: Int -> Pitch -> Pitch
increasePitch d = toPitch . (d+) . fromPitch

subtractPitch :: Pitch -> Pitch -> Int
subtractPitch (Pitch p0) (Pitch p1) = p1-p0

{- |
Convert pitch to frequency
according to the default tuning
given in MIDI 1.0 Detailed Specification.
-}
frequencyFromPitch :: (Floating a) => Pitch -> a
frequencyFromPitch (Pitch p) =
   440 * 2 ** (fromIntegral (p + 3 - 6*12) / 12)


instance Bounded Pitch where
   minBound = Pitch   0
   maxBound = Pitch 127

{- |
ToDo:
We have defined minBound = Velocity 0,
but strictly spoken the minimum Velocity is 1,
since Velocity zero means NoteOff.
One can at least think of NoteOff with (Velocity 0),
but I have never seen that.
-}
instance Bounded Velocity where
   minBound = Velocity   0
   maxBound = Velocity 127

instance Bounded Program where
   minBound = Program   0
   maxBound = Program 127


{- |
A MIDI problem is that one cannot uniquely map
a MIDI key to a frequency.
The frequency depends on the instrument.
I don't know if the deviations are defined for General MIDI.
If this applies one could add transposition information
to the use patch map.
For now I have chosen a value that leads to the right frequency
for some piano sound in my setup.
-}

zeroKey :: Pitch
zeroKey = toPitch 48

{- |
The velocity of an ordinary key stroke and
the maximum possible velocity.
-}
normalVelocity, maximumVelocity :: Velocity
normalVelocity  = Velocity 64
maximumVelocity = maxBound

{- |
MIDI specification says,
if velocity is simply mapped to amplitude,
then this should be done by an exponential function.
Thus we map 'normalVelocity' (64) to 0,
'maximumVelocity' (127) to 1,
and 'minimumVelocity' (1) to -1.
That is, normally you should write something like
@amplitude = 2 ** realFromVelocity vel@ or @3 ** realFromVelocity vel@.
-}
realFromVelocity :: (Fractional b) => Velocity -> b
realFromVelocity (Velocity x) =
   fromIntegral (x - fromVelocity normalVelocity) /
   fromIntegral (fromVelocity maximumVelocity - fromVelocity normalVelocity)


maximumControllerValue :: Num a => a
maximumControllerValue = 127

{- |
Map integral MIDI controller value to floating point value.
Maximum integral MIDI controller value 127 is mapped to 1.
Minimum integral MIDI controller value 0 is mapped to 0.
-}
realFromControllerValue :: (Integral a, Fractional b) => a -> b
realFromControllerValue x = fromIntegral x / maximumControllerValue



{-
These definitions will be deprecated
and then replaced by the ones from MIDI.Controller.
-}


-- * predefined MIDI controllers


-- ** simple names for controllers, if only most-significant bytes are used

bankSelect, modulation, breathControl, footControl, portamentoTime,
   dataEntry, mainVolume, balance, panorama, expression,
   generalPurpose1, generalPurpose2, generalPurpose3, generalPurpose4,
   vectorX, vectorY :: Ctrl.T
bankSelect      = bankSelectMSB
modulation      = modulationMSB
breathControl   = breathControlMSB
footControl     = footControlMSB
portamentoTime  = portamentoTimeMSB
dataEntry       = dataEntryMSB
mainVolume      = mainVolumeMSB
balance         = balanceMSB
panorama        = panoramaMSB
expression      = expressionMSB
generalPurpose1 = generalPurpose1MSB
generalPurpose2 = generalPurpose2MSB
generalPurpose3 = generalPurpose3MSB
generalPurpose4 = generalPurpose4MSB

vectorX = generalPurpose1
vectorY = generalPurpose2



-- ** controllers for most-significant bytes of control values
bankSelectMSB, modulationMSB, breathControlMSB, footControlMSB,
  portamentoTimeMSB, dataEntryMSB,
  mainVolumeMSB, balanceMSB, panoramaMSB, expressionMSB,
  generalPurpose1MSB, generalPurpose2MSB,
  generalPurpose3MSB, generalPurpose4MSB :: Ctrl.T

-- ** controllers for least-significant bytes of control values
bankSelectLSB, modulationLSB, breathControlLSB, footControlLSB,
  portamentoTimeLSB, dataEntryLSB,
  mainVolumeLSB, balanceLSB, panoramaLSB, expressionLSB,
  generalPurpose1LSB, generalPurpose2LSB,
  generalPurpose3LSB, generalPurpose4LSB :: Ctrl.T

-- ** additional single byte controllers
sustain, porta, sustenuto, softPedal, hold2,
  generalPurpose5, generalPurpose6, generalPurpose7, generalPurpose8,
  extDepth, tremoloDepth, chorusDepth, celesteDepth, phaserDepth :: Ctrl.T

-- ** increment/decrement and parameter numbers
dataIncrement, dataDecrement,
  nonRegisteredParameterLSB, nonRegisteredParameterMSB,
  registeredParameterLSB, registeredParameterMSB :: Ctrl.T


bankSelectMSB             = toEnum 0x00  {-  00 00 -}
modulationMSB             = toEnum 0x01  {-  01 01 -}
breathControlMSB          = toEnum 0x02  {-  02 02 -}
footControlMSB            = toEnum 0x04  {-  04 04 -}
portamentoTimeMSB         = toEnum 0x05  {-  05 05 -}
dataEntryMSB              = toEnum 0x06  {-  06 06 -}
mainVolumeMSB             = toEnum 0x07  {-  07 07 -}
balanceMSB                = toEnum 0x08  {-  08 08 -}
panoramaMSB               = toEnum 0x0A  {-  10 0A -}
expressionMSB             = toEnum 0x0B  {-  11 0B -}
generalPurpose1MSB        = toEnum 0x10  {-  16 10 -}
generalPurpose2MSB        = toEnum 0x11  {-  17 11 -}
generalPurpose3MSB        = toEnum 0x12  {-  18 12 -}
generalPurpose4MSB        = toEnum 0x13  {-  19 13 -}

bankSelectLSB             = toEnum 0x20  {-  32 20 -}
modulationLSB             = toEnum 0x21  {-  33 21 -}
breathControlLSB          = toEnum 0x22  {-  34 22 -}
footControlLSB            = toEnum 0x24  {-  36 24 -}
portamentoTimeLSB         = toEnum 0x25  {-  37 25 -}
dataEntryLSB              = toEnum 0x26  {-  38 26 -}
mainVolumeLSB             = toEnum 0x27  {-  39 27 -}
balanceLSB                = toEnum 0x28  {-  40 28 -}
panoramaLSB               = toEnum 0x2A  {-  42 2A -}
expressionLSB             = toEnum 0x2B  {-  43 2B -}
generalPurpose1LSB        = toEnum 0x30  {-  48 30 -}
generalPurpose2LSB        = toEnum 0x31  {-  49 31 -}
generalPurpose3LSB        = toEnum 0x32  {-  50 32 -}
generalPurpose4LSB        = toEnum 0x33  {-  51 33 -}

sustain                   = toEnum 0x40  {-  64 40 -}
porta                     = toEnum 0x41  {-  65 41 -}
sustenuto                 = toEnum 0x42  {-  66 42 -}
softPedal                 = toEnum 0x43  {-  67 43 -}
hold2                     = toEnum 0x45  {-  69 45 -}
generalPurpose5           = toEnum 0x50  {-  80 50 -}
generalPurpose6           = toEnum 0x51  {-  81 51 -}
generalPurpose7           = toEnum 0x52  {-  82 52 -}
generalPurpose8           = toEnum 0x53  {-  83 53 -}
extDepth                  = toEnum 0x5B  {-  91 5B -}
tremoloDepth              = toEnum 0x5C  {-  92 5C -}
chorusDepth               = toEnum 0x5D  {-  93 5D -}
celesteDepth              = toEnum 0x5E  {-  94 5E -}
phaserDepth               = toEnum 0x5F  {-  95 5F -}

dataIncrement             = toEnum 0x60  {-  96 60 -}
dataDecrement             = toEnum 0x61  {-  97 61 -}
nonRegisteredParameterLSB = toEnum 0x62  {-  98 62 -}
nonRegisteredParameterMSB = toEnum 0x63  {-  99 63 -}
registeredParameterLSB    = toEnum 0x64  {- 100 64 -}
registeredParameterMSB    = toEnum 0x65  {- 101 65 -}


-- * serialization

get :: Parser.C parser => Int -> Int -> Parser.Fragile parser T
get code firstData =
   let pitch  = toPitch firstData
       getVel = liftM toVelocity get1
   in  case code of
          08 -> liftM (NoteOff        pitch) getVel
          09 -> liftM (NoteOn         pitch) getVel
          10 -> liftM (PolyAftertouch pitch) get1
          {-
          Whether firstData is a controller and not a mode
          is checked in Message.Channel.get.
          -}
          11 -> liftM (Control (toEnum firstData)) get1
          12 -> return (ProgramChange (toProgram firstData))
          13 -> return (MonoAftertouch firstData)
          14 -> liftM (\msb -> PitchBend (firstData+128*msb)) get1
          _  -> Parser.giveUp ("invalid Voice message code:" ++ show code)


putWithStatus :: Writer.C writer =>
   (Int -> StatusWriter.T compress writer) ->
   T -> StatusWriter.T compress writer
putWithStatus putChan e =
   let putC code bytes =
          putChan code +#+
          StatusWriter.fromWriter (Writer.putByteList (map fromIntegral bytes))
   in  case e of
          NoteOff        p  v  -> putC  8 [fromPitch p, fromVelocity v]
          NoteOn         p  v  -> putC  9 [fromPitch p, fromVelocity v]
          PolyAftertouch p  pr -> putC 10 [fromPitch p, pr]
          Control        cn cv -> putC 11 [fromEnum cn, cv]
          ProgramChange  pn -> putC 12 [fromProgram pn]
          MonoAftertouch pr -> putC 13 [pr]
          PitchBend      pb ->
             let (hi,lo) = Bit.splitAt 7 pb in putC 14 [lo,hi] -- little-endian!!
