--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Marshal.Port
-- Copyright : (c) Henning Thielemann, 2011
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- PRIVATE MODULE.
--
-- Here we have the various types used by the library,
-- and how they are imported\/exported to C.
--
-- We use Hsc for expanding C types to Haskell types like Word32.
-- However if a C type is translated to Word32
-- you should not assume that it is translated to Word32 on every platform.
-- On a 64bit machine it may well be Word64.
-- Thus you should use our wrapper types whereever possible.
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.ALSA.Sequencer.Marshal.Port where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Marshal/Template.h>

import qualified Sound.ALSA.Sequencer.Utility as U
import qualified Foreign.C.Types as C
import qualified Data.Word as Word
import Foreign.Storable (Storable, )
import Data.Ix (Ix, range, index, inRange, rangeSize, )
import Data.Maybe (fromMaybe, )
import qualified Data.Ix.Enum as IxEnum
import qualified Data.EnumBitSet as EnumSet


-- The type of client ports.
newtype T =
   Cons #{intfieldtype snd_seq_addr_t, port}
      deriving (Eq, Ord, Storable)

instance Show T where
   showsPrec prec (Cons x) =
      U.showsRecord prec "Port" [U.showsField x]


exp :: T -> C.CInt
exp (Cons p) = fromIntegral p

imp :: C.CInt -> T
imp p = Cons (fromIntegral p)


{-
We could also define

> newtype CapabilityFlag = CapabilityFlag Int

but the enumeration definition gives nicer Show instance.
-}
data CapabilityFlag =
     CapOther Int   {- ^ a capability that cannot be represented by the other constructors -}
   | CapRead        {- ^ readable from this port -}
   | CapWrite       {- ^ writable to this port -}
   | CapSyncRead    {- ^ allow read subscriptions -}
   | CapSyncWrite   {- ^ allow write subscriptions -}
   | CapDuplex      {- ^ allow read/write duplex -}
   | CapSubsRead    {- ^ allow read subscription -}
   | CapSubsWrite   {- ^ allow write subscription -}
   | CapNoExport    {- ^ routing not allowed -}
     deriving (Eq, Ord, Show)

data TypeFlag =
     TypeOther Int      {- ^ a type that cannot be represented by the other constructors -}
   | TypeSpecific       {- ^ hardware specific -}
   | TypeMIDIGeneric    {- ^ generic MIDI device -}
   | TypeMIDIGM         {- ^ General MIDI compatible device -}
   | TypeMIDIGS         {- ^ GS compatible device -}
   | TypeMIDIXG         {- ^ XG compatible device -}
   | TypeMIDIMT32       {- ^ MT-32 compatible device -}
   | TypeMIDIGM2        {- ^ General MIDI 2 compatible device -}

   | TypeSynth          {- ^ Synth device -}
   | TypeDirectSample   {- ^ Sampling device (support sample download) -}
   | TypeSample         {- ^ Sampling device (sample can be downloaded at any time) -}

   | TypeHardware       {- ^ This port is implemented in hardware. -}
   | TypeSoftware       {- ^ This port is implemented in software. -}
   | TypeSynthesizer    {- ^ Messages sent to this port will generate sounds. -}
   | TypePort           {- ^ This port may connect to other devices
                             (whose characteristics are not known). -}
   | TypeApplication    {- ^ application (sequencer/editor) -}
     deriving (Eq, Ord, Show)


capFlagSet :: CapabilityFlag -> Cap
capFlagSet cap =
   case cap of
      CapOther n   -> EnumSet.singletonByPosition n
      CapRead      -> capRead
      CapWrite     -> capWrite
      CapSyncRead  -> capSyncRead
      CapSyncWrite -> capSyncWrite
      CapDuplex    -> capDuplex
      CapSubsRead  -> capSubsRead
      CapSubsWrite -> capSubsWrite
      CapNoExport  -> capNoExport

{- |
The Enum instance may not be very efficient,
but it should hardly be used, at all.
Better use constants such as 'capRead' and set manipulation.
If the binary logarithm is computed by constant unfolding,
performance would be better, but direct set manipulation is still faster.
We implement the 'Enum' instance in this way,
in order to stay independent from the particular ALSA definitions,
that may differ between platforms.
-}
instance Enum CapabilityFlag where
   fromEnum cap =
      case cap of
         CapOther n -> n
         _ -> EnumSet.mostSignificantPosition (capFlagSet cap)
   toEnum n =
      fromMaybe (CapOther n) $
      lookup (EnumSet.singletonByPosition n) $
      map (\cap -> (capFlagSet cap, cap)) $
         CapRead :
         CapWrite :
         CapSyncRead :
         CapSyncWrite :
         CapDuplex :
         CapSubsRead :
         CapSubsWrite :
         CapNoExport :
         []

instance Ix CapabilityFlag where
   range     = IxEnum.range
   index     = IxEnum.index
   inRange   = IxEnum.inRange
   rangeSize = IxEnum.rangeSize


typeFlagSet :: TypeFlag -> Type
typeFlagSet typ =
   case typ of
      TypeOther n      -> EnumSet.singletonByPosition n
      TypeSpecific     -> typeSpecific
      TypeMIDIGeneric  -> typeMidiGeneric
      TypeMIDIGM       -> typeMidiGM
      TypeMIDIGS       -> typeMidiGS
      TypeMIDIXG       -> typeMidiXG
      TypeMIDIMT32     -> typeMidiMT32
      TypeMIDIGM2      -> typeMidiGM2
      TypeSynth        -> typeSynth
      TypeDirectSample -> typeDirectSample
      TypeSample       -> typeSample
      TypeHardware     -> typeHardware
      TypeSoftware     -> typeSoftware
      TypeSynthesizer  -> typeSynthesizer
      TypePort         -> typePort
      TypeApplication  -> typeApplication

instance Enum TypeFlag where
   fromEnum typ =
      case typ of
         TypeOther n -> n
         _ -> EnumSet.mostSignificantPosition (typeFlagSet typ)
   toEnum n =
      fromMaybe (TypeOther n) $
      lookup (EnumSet.singletonByPosition n) $
      map (\typ -> (typeFlagSet typ, typ)) $
         TypeSpecific :
         TypeMIDIGeneric :
         TypeMIDIGM :
         TypeMIDIGS :
         TypeMIDIXG :
         TypeMIDIMT32 :
         TypeMIDIGM2 :
         TypeSynth :
         TypeDirectSample :
         TypeSample :
         TypeHardware :
         TypeSoftware :
         TypeSynthesizer :
         TypePort :
         TypeApplication :
         []


instance Ix TypeFlag where
   range     = IxEnum.range
   index     = IxEnum.index
   inRange   = IxEnum.inRange
   rangeSize = IxEnum.rangeSize


-- | Port capabilities.
type Cap = EnumSet.T C.CUInt CapabilityFlag

-- | Port types.
type Type = EnumSet.T C.CUInt TypeFlag

#{enum T, Cons
 , systemTimer    = SND_SEQ_PORT_SYSTEM_TIMER
 , systemAnnounce = SND_SEQ_PORT_SYSTEM_ANNOUNCE
 , unknown        = SND_SEQ_ADDRESS_UNKNOWN
 }

#{enum Cap, EnumSet.Cons
 , capRead      = SND_SEQ_PORT_CAP_READ
 , capWrite     = SND_SEQ_PORT_CAP_WRITE
 , capSyncRead  = SND_SEQ_PORT_CAP_SYNC_READ
 , capSyncWrite = SND_SEQ_PORT_CAP_SYNC_WRITE
 , capDuplex    = SND_SEQ_PORT_CAP_DUPLEX
 , capSubsRead  = SND_SEQ_PORT_CAP_SUBS_READ
 , capSubsWrite = SND_SEQ_PORT_CAP_SUBS_WRITE
 , capNoExport  = SND_SEQ_PORT_CAP_NO_EXPORT
 }

caps :: [Cap] -> Cap
caps = EnumSet.unions

#{enum Type, EnumSet.Cons
 , typeSpecific     = SND_SEQ_PORT_TYPE_SPECIFIC
 , typeMidiGeneric  = SND_SEQ_PORT_TYPE_MIDI_GENERIC
 , typeMidiGM       = SND_SEQ_PORT_TYPE_MIDI_GM
 , typeMidiGS       = SND_SEQ_PORT_TYPE_MIDI_GS
 , typeMidiXG       = SND_SEQ_PORT_TYPE_MIDI_XG
 , typeMidiMT32     = SND_SEQ_PORT_TYPE_MIDI_MT32
 , typeMidiGM2      = SND_SEQ_PORT_TYPE_MIDI_GM2

 , typeSynth        = SND_SEQ_PORT_TYPE_SYNTH
 , typeDirectSample = SND_SEQ_PORT_TYPE_DIRECT_SAMPLE
 , typeSample       = SND_SEQ_PORT_TYPE_SAMPLE

 , typeHardware     = SND_SEQ_PORT_TYPE_HARDWARE
 , typeSoftware     = SND_SEQ_PORT_TYPE_SOFTWARE
 , typeSynthesizer  = SND_SEQ_PORT_TYPE_SYNTHESIZER
 , typePort         = SND_SEQ_PORT_TYPE_PORT
 , typeApplication  = SND_SEQ_PORT_TYPE_APPLICATION
 }

types :: [Type] -> Type
types = EnumSet.unions
