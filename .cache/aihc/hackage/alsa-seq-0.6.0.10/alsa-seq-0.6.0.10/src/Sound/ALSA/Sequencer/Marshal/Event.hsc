--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Marshal.Event
-- Copyright : (c) Henning Thielemann, 2011-2012
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
module Sound.ALSA.Sequencer.Marshal.Event where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Marshal/Template.h>

import qualified Sound.ALSA.Sequencer.Marshal.Connect as Connect
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Marshal.Time as Time

import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString as B

import qualified Foreign.Marshal.Alloc as MA
import qualified Foreign.C.Types as C
import qualified Data.Int as Int
import qualified Data.Word as Word
import Foreign.Storable
          (Storable, sizeOf, alignment, peek, poke, pokeByteOff, peekByteOff, )
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr, )
import Control.Monad (liftM2, )
import Data.Monoid (mappend, )
import qualified Data.FlagSet as FlagSet
import Data.Array (Ix, Array, (!), accumArray, )
import Data.Tuple.HT (mapFst, )


data Flag
type Flags     = FlagSet.T #{intfieldtype snd_seq_event_t, flags} Flag
type FlagValue = FlagSet.MaskedValue #{intfieldtype snd_seq_event_t, flags} Flag

instance Time.Flag Flag where


eventLengthFixed, eventLengthVariable, eventLengthVarUser :: FlagValue
eventLengthFixed =
   FlagSet.MaskedValue
      #{const SND_SEQ_EVENT_LENGTH_MASK}
      #{const SND_SEQ_EVENT_LENGTH_FIXED}
eventLengthVariable =
   FlagSet.MaskedValue
      #{const SND_SEQ_EVENT_LENGTH_MASK}
      #{const SND_SEQ_EVENT_LENGTH_VARIABLE}
eventLengthVarUser =
   FlagSet.MaskedValue
      #{const SND_SEQ_EVENT_LENGTH_MASK}
      #{const SND_SEQ_EVENT_LENGTH_VARUSR}


priorityHigh, priorityNormal :: FlagValue
priorityHigh =
   FlagSet.MaskedValue
      #{const SND_SEQ_PRIORITY_MASK}
      #{const SND_SEQ_PRIORITY_HIGH}
priorityNormal =
   FlagSet.MaskedValue
      #{const SND_SEQ_PRIORITY_MASK}
      #{const SND_SEQ_PRIORITY_NORMAL}



{- available in alsa-1.0.14, but gone in 1.0.22
#{newinttype "InstrCluster", snd_seq_instr_cluster_t}

#{newintfieldtype "StandardId", snd_seq_instr_t, std}
#{newintfieldtype "Bank",       snd_seq_instr_t, bank}
#{newintfieldtype "Program",    snd_seq_instr_t, prg}

data Instr = Instr
   { instrCluster :: !InstrCluster
    -- XXX: perhaps use Sample?
   , instrStd     :: !StandardId
   , instrBank    :: !Bank
   , instrPrg     :: !Program
   } deriving (Show)

instance Storable Instr where
  sizeOf _    = #{size snd_seq_instr_t}
  alignment _ = #{alignment snd_seq_instr_t}
  peek p      = do cl <- #{peek snd_seq_instr_t, cluster} p
                   st <- #{peek snd_seq_instr_t, std} p
                   ba <- #{peek snd_seq_instr_t, bank} p
                   pr <- #{peek snd_seq_instr_t, prg} p
                   return Instr { instrCluster = cl
                                , instrStd     = st
                                , instrBank    = ba
                                , instrPrg     = pr
                                }
  poke p v    = #{poke snd_seq_instr_t, cluster} p (instrCluster v)
             >> #{poke snd_seq_instr_t, std}     p (instrStd v)
             >> #{poke snd_seq_instr_t, bank}    p (instrBank v)
             >> #{poke snd_seq_instr_t, prg}     p (instrPrg v)
-}


#{newintfieldtype "Channel",  snd_seq_ev_note_t, channel}
#{newintfieldtype "Pitch",    snd_seq_ev_note_t, note}
#{newintfieldtype "Velocity", snd_seq_ev_note_t, velocity}
#{newintfieldtype "Duration", snd_seq_ev_note_t, duration}

data Note = Note
   { noteChannel      :: !Channel
   , noteNote         :: !Pitch
   , noteVelocity     :: !Velocity
   , noteOffVelocity  :: !Velocity
   , noteDuration     :: !Duration
   } deriving (Show)


instance Storable Note where
  sizeOf _    = #{size snd_seq_ev_note_t}
  alignment _ = #{alignment snd_seq_ev_note_t}
  peek p      = do c  <- #{peek snd_seq_ev_note_t, channel} p
                   n  <- #{peek snd_seq_ev_note_t, note} p
                   v  <- #{peek snd_seq_ev_note_t, velocity} p
                   ov <- #{peek snd_seq_ev_note_t, off_velocity} p
                   d  <- #{peek snd_seq_ev_note_t, duration} p
                   return Note { noteChannel = c
                               , noteNote = n
                               , noteVelocity = v
                               , noteOffVelocity = ov
                               , noteDuration = d
                               }
  poke p v    = #{poke snd_seq_ev_note_t, channel}      p (noteChannel v)
             >> #{poke snd_seq_ev_note_t, note}         p (noteNote v)
             >> #{poke snd_seq_ev_note_t, velocity}     p (noteVelocity v)
             >> #{poke snd_seq_ev_note_t, off_velocity} p (noteOffVelocity v)
             >> #{poke snd_seq_ev_note_t, duration}     p (noteDuration v)


#{newintfieldtype "Parameter", snd_seq_ev_ctrl_t, param}
#{newintfieldtype "Value",     snd_seq_ev_ctrl_t, value}

data Ctrl = Ctrl
   { ctrlChannel  :: !Channel
   , ctrlParam    :: !Parameter
   , ctrlValue    :: !Value
   } deriving (Show)

instance Storable Ctrl where
  sizeOf _    = #{size snd_seq_ev_ctrl_t}
  alignment _ = #{alignment snd_seq_ev_ctrl_t}
  peek p      = do ct <- #{peek snd_seq_ev_ctrl_t, channel} p
                   pa <- #{peek snd_seq_ev_ctrl_t, param} p
                   va <- #{peek snd_seq_ev_ctrl_t, value} p
                   return Ctrl { ctrlChannel = ct
                               , ctrlParam   = pa
                               , ctrlValue   = va
                               }
  poke p v    = #{poke snd_seq_ev_ctrl_t, channel} p (ctrlChannel v)
             >> #{poke snd_seq_ev_ctrl_t, param}   p (ctrlParam v)
             >> #{poke snd_seq_ev_ctrl_t, value}   p (ctrlValue v)


pokeQueue ::
  Ptr Data -> QueueEv -> Queue.T -> IO EType
pokeQueue p e q = do
  poke (castPtr p) q
  case e of
    QueueSetPosTick t -> #{poke snd_seq_ev_queue_control_t, param.time.tick} p t
    QueueSetPosTime t -> #{poke snd_seq_ev_queue_control_t, param.time.time} p t
    QueueTempo      t -> #{poke snd_seq_ev_queue_control_t, param.value} p t
    QueueSkew       s -> #{poke snd_seq_ev_queue_control_t, param.skew} p s
    QueueSyncPos    s -> #{poke snd_seq_ev_queue_control_t, param.position} p s
    _ -> return ()
  return (expEv e)


{- available in alsa-1.0.14, but gone in 1.0.22
data Sample = Sample
   { sampleStd  :: !StandardId
   , sampleBank :: !Bank
   , samplePrg  :: !Program
   } deriving (Show)

instance Storable Sample where
  sizeOf _    = #{size snd_seq_ev_sample_t}
  alignment _ = #{alignment snd_seq_ev_sample_t}
  peek p      = do st <- #{peek snd_seq_ev_sample_t, std} p
                   ba <- #{peek snd_seq_ev_sample_t, bank} p
                   pr <- #{peek snd_seq_ev_sample_t, prg} p
                   return Sample { sampleStd     = st
                                 , sampleBank    = ba
                                 , samplePrg     = pr
                                 }
  poke p v    = #{poke snd_seq_ev_sample_t, std}     p (sampleStd v)
             >> #{poke snd_seq_ev_sample_t, bank}    p (sampleBank v)
             >> #{poke snd_seq_ev_sample_t, prg}     p (samplePrg v)


newtype Cluster = Cluster
   { clusterCluster :: InstrCluster
   } deriving (Show, Eq, Storable)


#{newintfieldtype "Volume",  snd_seq_ev_volume_t, volume}
#{newintfieldtype "Balance", snd_seq_ev_volume_t, lr}

-- | These are all 14 bit values.
data VolumeControl = VolumeControl
   { volumeVolume  :: !Volume
   , volumeLR      :: !Balance
   , volumeFR      :: !Balance
   , volumeDU      :: !Balance
   } deriving (Show)

instance Storable VolumeControl where
  sizeOf _    = #{size snd_seq_ev_volume_t}
  alignment _ = #{alignment snd_seq_ev_volume_t}
  peek p      = do v <- #{peek snd_seq_ev_volume_t, volume} p
                   l <- #{peek snd_seq_ev_volume_t, lr} p
                   f <- #{peek snd_seq_ev_volume_t, fr} p
                   d <- #{peek snd_seq_ev_volume_t, du} p
                   return VolumeControl
                                 { volumeVolume  = v
                                 , volumeLR      = l
                                 , volumeFR      = f
                                 , volumeDU      = d
                                 }
  poke p v    = #{poke snd_seq_ev_volume_t, volume} p (volumeVolume v)
             >> #{poke snd_seq_ev_volume_t, lr}     p (volumeLR v)
             >> #{poke snd_seq_ev_volume_t, fr}     p (volumeFR v)
             >> #{poke snd_seq_ev_volume_t, du}     p (volumeDU v)
-}


data Custom =
  Custom {
    custom0, custom1, custom2 :: !(#{intfieldtype snd_seq_ev_raw32_t, d[0]})
  } deriving (Show)

instance Storable Custom where
  sizeOf _    = #{size snd_seq_ev_raw32_t}
  alignment _ = #{alignment snd_seq_ev_raw32_t}
  peek p      = do d0 <- #{peek snd_seq_ev_raw32_t, d[0]} p
                   d1 <- #{peek snd_seq_ev_raw32_t, d[1]} p
                   d2 <- #{peek snd_seq_ev_raw32_t, d[2]} p
                   return Custom { custom0 = d0
                                 , custom1 = d1
                                 , custom2 = d2
                                 }
  poke p v    = #{poke snd_seq_ev_raw32_t, d[0]} p (custom0 v)
             >> #{poke snd_seq_ev_raw32_t, d[1]} p (custom1 v)
             >> #{poke snd_seq_ev_raw32_t, d[2]} p (custom2 v)


#{newintfieldtype "Tag", snd_seq_event_t, tag}

data T = Cons
   { highPriority   :: !Bool
   , tag            :: !Tag
   , queue          :: !Queue.T
   , time           :: !Time.T
   , source         :: !Addr.T
   , dest           :: !Addr.T
   , body           :: !Data
   } deriving Show

instance Storable T where
  sizeOf _    = #{size snd_seq_event_t}
  alignment _ = #{alignment snd_seq_event_t}
  peek p =
    do ty    <- #{peek snd_seq_event_t, type} p
       flags <- #{peek snd_seq_event_t, flags} p
       tg    <- #{peek snd_seq_event_t, tag} p
       q     <- #{peek snd_seq_event_t, queue} p
       t     <- Time.peek flags (#{ptr snd_seq_event_t, time} p)
       src   <- #{peek snd_seq_event_t, source} p
       dst   <- #{peek snd_seq_event_t, dest} p
       d     <- (peekData ! ty) (#{ptr snd_seq_event_t, data} p)
       return Cons
         { highPriority = not $ FlagSet.match flags priorityNormal
         , tag = tg
         , queue = q
         , time = t
         , source = src
         , dest = dst
         , body = d
         }
  poke p e =
    pokeHeader p e eventLengthFixed
      =<< pokeData (#{ptr snd_seq_event_t, data} p) (body e)

withMaybe :: Maybe T -> (Ptr T -> IO a) -> IO a
withMaybe me f =
  maybe (f nullPtr) (flip with f) me

with :: T -> (Ptr T -> IO a) -> IO a
with ev f =
  MA.alloca $ \p ->
    case body ev of
      ExtEv e d -> do
        let lengthFlag =
              case e of
                SysEx -> eventLengthVariable
                Bounce -> eventLengthVariable
                _ -> eventLengthVarUser
        pokeHeader p ev lengthFlag (expEv e)
        BU.unsafeUseAsCString d $ \ptr -> do
          #{poke snd_seq_event_t, data.ext.len} p (B.length d)
          #{poke snd_seq_event_t, data.ext.ptr} p ptr
          f p
      b -> do
        pokeHeader p ev eventLengthFixed
          =<< pokeData (#{ptr snd_seq_event_t, data} p) b
        f p


pokeBody ::
  (Storable d, Type e) =>
  Ptr Data -> e -> d -> IO EType
pokeBody p e d =
  poke (castPtr p) d >> return (expEv e)

pokeData :: Ptr Data -> Data -> IO EType
pokeData p dt = case dt of
  NoteEv   e d -> pokeBody p e d
  CtrlEv   e d -> pokeBody p e d
  QueueEv  e d -> pokeQueue p e d
  AddrEv   e d -> pokeBody p e d
  ConnEv   e d -> pokeBody p e d
  CustomEv e d -> pokeBody p e d
  ExtEv    _ _ -> error "cannot simply poke ExtEv, because it needs allocation - use Event.with instead"
  EmptyEv  e   -> return (expEv e)

pokeHeader :: Ptr T -> T -> FlagValue -> EType -> IO ()
pokeHeader p e lengthFlag ty = do
  #{poke snd_seq_event_t, type} p ty
  #{poke snd_seq_event_t, tag} p (tag e)
  #{poke snd_seq_event_t, queue} p (queue e)
  real <- Time.poke (#{ptr snd_seq_event_t, time} p) (time e)
  #{poke snd_seq_event_t, source} p (source e)
  #{poke snd_seq_event_t, dest} p (dest e)
  let flags =
         (if highPriority e
            then priorityHigh
            else priorityNormal)
         `mappend` real
         `mappend` lengthFlag
  #{poke snd_seq_event_t, flags} p (FlagSet.fromMaskedValue flags)


peekData :: Array EType (Ptr Data -> IO Data)
peekData =
  accumArray (const id) unknown (EType 0, EType 255) $
  map (mapFst EType) $
  [ -- result events (2)
    (#{const SND_SEQ_EVENT_SYSTEM}, unknown)
  , (#{const SND_SEQ_EVENT_RESULT}, unknown)

    -- note events (4)
  , (#{const SND_SEQ_EVENT_NOTE},     peekNoteEv ANote)
  , (#{const SND_SEQ_EVENT_NOTEON},   peekNoteEv NoteOn)
  , (#{const SND_SEQ_EVENT_NOTEOFF},  peekNoteEv NoteOff)
  , (#{const SND_SEQ_EVENT_KEYPRESS}, peekNoteEv KeyPress)

    -- control events (12)
  , (#{const SND_SEQ_EVENT_CONTROLLER},  peekCtrlEv Controller)
  , (#{const SND_SEQ_EVENT_PGMCHANGE},   peekCtrlEv PgmChange)
  , (#{const SND_SEQ_EVENT_CHANPRESS},   peekCtrlEv ChanPress)
  , (#{const SND_SEQ_EVENT_PITCHBEND},   peekCtrlEv PitchBend)
  , (#{const SND_SEQ_EVENT_CONTROL14},   peekCtrlEv Control14)
  , (#{const SND_SEQ_EVENT_NONREGPARAM}, peekCtrlEv NonRegParam)
  , (#{const SND_SEQ_EVENT_REGPARAM},    peekCtrlEv RegParam)
  , (#{const SND_SEQ_EVENT_SONGPOS},     peekCtrlEv SongPos)
  , (#{const SND_SEQ_EVENT_SONGSEL},     peekCtrlEv SongSel)
  , (#{const SND_SEQ_EVENT_QFRAME},      peekCtrlEv QFrame)
  , (#{const SND_SEQ_EVENT_TIMESIGN},    peekCtrlEv TimeSign)
  , (#{const SND_SEQ_EVENT_KEYSIGN},     peekCtrlEv KeySign)

  -- queue control (10)
  , (#{const SND_SEQ_EVENT_START},       peekQueueEv  QueueStart)
  , (#{const SND_SEQ_EVENT_CONTINUE},    peekQueueEv  QueueContinue)
  , (#{const SND_SEQ_EVENT_STOP},        peekQueueEv  QueueStop)
  , (#{const SND_SEQ_EVENT_SETPOS_TICK}, peekQueueExt QueueSetPosTick #{peekintfield snd_seq_ev_queue_control_t, param.time.tick})
  , (#{const SND_SEQ_EVENT_SETPOS_TIME}, peekQueueExt QueueSetPosTime #{peek snd_seq_ev_queue_control_t, param.time.time})
  , (#{const SND_SEQ_EVENT_TEMPO},       peekQueueExt QueueTempo      #{peek snd_seq_ev_queue_control_t, param.value})
  , (#{const SND_SEQ_EVENT_CLOCK},       peekQueueEv  QueueClock)
  , (#{const SND_SEQ_EVENT_TICK},        peekQueueEv  QueueTick)
  , (#{const SND_SEQ_EVENT_QUEUE_SKEW},  peekQueueExt QueueSkew       #{peek snd_seq_ev_queue_control_t, param.skew})
  , (#{const SND_SEQ_EVENT_SYNC_POS},    peekQueueExt QueueSyncPos    #{peek snd_seq_ev_queue_control_t, param.position})

  -- misc (3)
  , (#{const SND_SEQ_EVENT_TUNE_REQUEST}, peekEmptyEv TuneRequest)
  , (#{const SND_SEQ_EVENT_RESET},        peekEmptyEv Reset)
  , (#{const SND_SEQ_EVENT_SENSING},      peekEmptyEv Sensing)

  , (#{const SND_SEQ_EVENT_ECHO}, peekCustomEv Echo)
  , (#{const SND_SEQ_EVENT_OSS},  peekCustomEv OSS)

  -- networking (8)
  , (#{const SND_SEQ_EVENT_CLIENT_START},  peekAddrEv ClientStart)
  , (#{const SND_SEQ_EVENT_CLIENT_EXIT},   peekAddrEv ClientExit)
  , (#{const SND_SEQ_EVENT_CLIENT_CHANGE}, peekAddrEv ClientChange)
  , (#{const SND_SEQ_EVENT_PORT_START},    peekAddrEv PortStart)
  , (#{const SND_SEQ_EVENT_PORT_EXIT},     peekAddrEv PortExit)
  , (#{const SND_SEQ_EVENT_PORT_CHANGE},   peekAddrEv PortChange)
  , (#{const SND_SEQ_EVENT_PORT_SUBSCRIBED},   peekConnEv PortSubscribed)
  , (#{const SND_SEQ_EVENT_PORT_UNSUBSCRIBED}, peekConnEv PortUnsubscribed)

{- available in alsa-1.0.14, but gone in 1.0.22
  , (#{const SND_SEQ_EVENT_SAMPLE}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_CLUSTER}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_START}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_STOP}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_FREQ}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_VOLUME}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_LOOP}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_POSITION}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_PRIVATE1}, unknown)
-}
  , (#{const SND_SEQ_EVENT_USR0}, peekCustomEv User0)
  , (#{const SND_SEQ_EVENT_USR1}, peekCustomEv User1)
  , (#{const SND_SEQ_EVENT_USR2}, peekCustomEv User2)
  , (#{const SND_SEQ_EVENT_USR3}, peekCustomEv User3)
  , (#{const SND_SEQ_EVENT_USR4}, peekCustomEv User4)
  , (#{const SND_SEQ_EVENT_USR5}, peekCustomEv User5)
  , (#{const SND_SEQ_EVENT_USR6}, peekCustomEv User6)
  , (#{const SND_SEQ_EVENT_USR7}, peekCustomEv User7)
  , (#{const SND_SEQ_EVENT_USR8}, peekCustomEv User8)
  , (#{const SND_SEQ_EVENT_USR9}, peekCustomEv User9)

{- available in alsa-1.0.14, but gone in 1.0.22
  , (#{const SND_SEQ_EVENT_INSTR_BEGIN}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_END}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_INFO}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_INFO_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_FINFO}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_FINFO_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_RESET}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_STATUS}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_STATUS_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_PUT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_GET}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_GET_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_FREE}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_LIST}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_LIST_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_CLUSTER}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_CLUSTER_GET}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_CLUSTER_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_CHANGE}, unknown)
-}

  , (#{const SND_SEQ_EVENT_SYSEX},    peekExtEv SysEx   )
  , (#{const SND_SEQ_EVENT_BOUNCE},   peekExtEv Bounce  )
  , (#{const SND_SEQ_EVENT_USR_VAR0}, peekExtEv UserVar0)
  , (#{const SND_SEQ_EVENT_USR_VAR1}, peekExtEv UserVar1)
  , (#{const SND_SEQ_EVENT_USR_VAR2}, peekExtEv UserVar2)
  , (#{const SND_SEQ_EVENT_USR_VAR3}, peekExtEv UserVar3)
  , (#{const SND_SEQ_EVENT_USR_VAR3}, peekExtEv UserVar4)

  , (#{const SND_SEQ_EVENT_NONE}, peekEmptyEv None)
  ]

  where unknown = peekEmptyEv Unknown


data NoteEv   = ANote | NoteOn | NoteOff | KeyPress
                deriving (Show, Eq, Ord, Enum, Bounded)

data CtrlEv   = Controller | PgmChange | ChanPress
              | PitchBend | Control14
              | NonRegParam | RegParam
              | SongPos | SongSel
              | QFrame
              | TimeSign | KeySign
                deriving (Show, Eq, Ord, Enum, Bounded)

#{newintfieldtype "Tempo", snd_seq_ev_queue_control_t, param.value}

data QueueEv  = QueueStart
              | QueueContinue
              | QueueStop
              | QueueSetPosTick !Time.Tick
              | QueueSetPosTime !RealTime.T
              | QueueTempo      !Tempo
              | QueueClock
              | QueueTick
              | QueueSkew       !Queue.Skew
              | QueueSyncPos    !Queue.Position
                deriving (Show, Eq)

data EmptyEv  = TuneRequest | Reset | Sensing | None | Unknown
                deriving (Show, Eq, Ord, Enum, Bounded)

data CustomEv = Echo | OSS
              | User0 | User1 | User2 | User3 | User4
              | User5 | User6 | User7 | User8 | User9
                deriving (Show, Eq, Ord, Enum, Bounded)

data ExtEv    = SysEx | Bounce
              | UserVar0 | UserVar1 | UserVar2 | UserVar3 | UserVar4
                deriving (Show, Eq, Ord, Enum, Bounded)

data AddrEv   = ClientStart | ClientExit | ClientChange
              | PortStart | PortExit | PortChange
                deriving (Show, Eq, Ord, Enum, Bounded)

data ConnEv   = PortSubscribed | PortUnsubscribed
                deriving (Show, Eq, Ord, Enum, Bounded)


#{newinttype "EType", snd_seq_event_type_t}

-- type EType = #{inttype snd_seq_event_type_t}


class Type e where
  expEv :: e -> EType

instance Type NoteEv where
 expEv e = EType $ case e of
  ANote    -> #{const SND_SEQ_EVENT_NOTE}
  NoteOn   -> #{const SND_SEQ_EVENT_NOTEON}
  NoteOff  -> #{const SND_SEQ_EVENT_NOTEOFF}
  KeyPress -> #{const SND_SEQ_EVENT_KEYPRESS}

instance Type CtrlEv where
 expEv e = EType $ case e of
  Controller  -> #{const SND_SEQ_EVENT_CONTROLLER}
  PgmChange   -> #{const SND_SEQ_EVENT_PGMCHANGE}
  ChanPress   -> #{const SND_SEQ_EVENT_CHANPRESS}
  PitchBend   -> #{const SND_SEQ_EVENT_PITCHBEND}
  Control14   -> #{const SND_SEQ_EVENT_CONTROL14}
  NonRegParam -> #{const SND_SEQ_EVENT_NONREGPARAM}
  RegParam    -> #{const SND_SEQ_EVENT_REGPARAM}
  SongPos     -> #{const SND_SEQ_EVENT_SONGPOS}
  SongSel     -> #{const SND_SEQ_EVENT_SONGSEL}
  QFrame      -> #{const SND_SEQ_EVENT_QFRAME}
  TimeSign    -> #{const SND_SEQ_EVENT_TIMESIGN}
  KeySign     -> #{const SND_SEQ_EVENT_KEYSIGN}

instance Type QueueEv where
  expEv = fst . expQueueEv

-- setPosTick should be an error
expQueueEv :: QueueEv -> (EType, C.CInt)
expQueueEv e = mapFst EType $ case e of
  QueueStart        -> (#{const SND_SEQ_EVENT_START}       , 0)
  QueueContinue     -> (#{const SND_SEQ_EVENT_CONTINUE}    , 0)
  QueueStop         -> (#{const SND_SEQ_EVENT_STOP}        , 0)
  QueueSetPosTick _ -> (#{const SND_SEQ_EVENT_SETPOS_TICK} , error "expQueueEv.QueueSetPosTick: cannot represent position as int")
  QueueSetPosTime _ -> (#{const SND_SEQ_EVENT_SETPOS_TIME} , error "expQueueEv.QueueSetPosTime: cannot represent position as int")
  QueueTempo      x -> (#{const SND_SEQ_EVENT_TEMPO}       , fromIntegral $ unTempo x)
  QueueClock        -> (#{const SND_SEQ_EVENT_CLOCK}       , 0)
  QueueTick         -> (#{const SND_SEQ_EVENT_TICK}        , 0)
  QueueSkew       _ -> (#{const SND_SEQ_EVENT_QUEUE_SKEW}  , error "expQueueEv.QueueSkew: cannot represent skew record as int")
  QueueSyncPos    x -> (#{const SND_SEQ_EVENT_SYNC_POS}    , fromIntegral $ Queue.unPosition x)


instance Type EmptyEv where
 expEv e = EType $ case e of
  TuneRequest -> #{const SND_SEQ_EVENT_TUNE_REQUEST}
  Reset       -> #{const SND_SEQ_EVENT_RESET}
  Sensing     -> #{const SND_SEQ_EVENT_SENSING}
  None        -> #{const SND_SEQ_EVENT_NONE}
  Unknown     -> #{const SND_SEQ_EVENT_NONE}

instance Type CustomEv where
 expEv e = EType $ case e of
  Echo  -> #{const SND_SEQ_EVENT_ECHO}
  OSS   -> #{const SND_SEQ_EVENT_OSS}
  User0 -> #{const SND_SEQ_EVENT_USR0}
  User1 -> #{const SND_SEQ_EVENT_USR1}
  User2 -> #{const SND_SEQ_EVENT_USR2}
  User3 -> #{const SND_SEQ_EVENT_USR3}
  User4 -> #{const SND_SEQ_EVENT_USR4}
  User5 -> #{const SND_SEQ_EVENT_USR5}
  User6 -> #{const SND_SEQ_EVENT_USR6}
  User7 -> #{const SND_SEQ_EVENT_USR7}
  User8 -> #{const SND_SEQ_EVENT_USR8}
  User9 -> #{const SND_SEQ_EVENT_USR9}

instance Type ExtEv where
 expEv e = EType $ case e of
  SysEx    -> #{const SND_SEQ_EVENT_SYSEX}
  Bounce   -> #{const SND_SEQ_EVENT_BOUNCE}
  UserVar0 -> #{const SND_SEQ_EVENT_USR_VAR0}
  UserVar1 -> #{const SND_SEQ_EVENT_USR_VAR1}
  UserVar2 -> #{const SND_SEQ_EVENT_USR_VAR2}
  UserVar3 -> #{const SND_SEQ_EVENT_USR_VAR3}
  UserVar4 -> #{const SND_SEQ_EVENT_USR_VAR4}

instance Type AddrEv where
  expEv e = EType $ case e of
    ClientStart -> #{const SND_SEQ_EVENT_CLIENT_START}
    ClientExit -> #{const SND_SEQ_EVENT_CLIENT_EXIT}
    ClientChange -> #{const SND_SEQ_EVENT_CLIENT_CHANGE}
    PortStart -> #{const SND_SEQ_EVENT_PORT_START}
    PortExit -> #{const SND_SEQ_EVENT_PORT_EXIT}
    PortChange -> #{const SND_SEQ_EVENT_PORT_CHANGE}

instance Type ConnEv where
 expEv e = EType $ case e of
  PortSubscribed   -> #{const SND_SEQ_EVENT_PORT_SUBSCRIBED}
  PortUnsubscribed -> #{const SND_SEQ_EVENT_PORT_UNSUBSCRIBED}


maxEventType :: EmptyEv
maxEventType = maxBound


peekBody ::
  (Storable d) =>
  (d -> Data) -> Ptr Data -> IO Data
peekBody makeBody p =
  fmap makeBody (peek (castPtr p))

peekNoteEv :: NoteEv -> Ptr Data -> IO Data
peekNoteEv e = peekBody (NoteEv e)

peekCtrlEv :: CtrlEv -> Ptr Data -> IO Data
peekCtrlEv e = peekBody (CtrlEv e)

peekQueueEv :: QueueEv -> Ptr Data -> IO Data
peekQueueEv e = peekBody (QueueEv e)

peekQueueExt :: (a -> QueueEv) -> (Ptr Data -> IO a) -> Ptr Data -> IO Data
peekQueueExt makeQ peekParam p =
  liftM2 (QueueEv . makeQ) (peekParam p) (peek (castPtr p))

peekAddrEv :: AddrEv -> Ptr Data -> IO Data
peekAddrEv e = peekBody (AddrEv e)

peekConnEv :: ConnEv -> Ptr Data -> IO Data
peekConnEv e = peekBody (ConnEv e)

peekEmptyEv :: EmptyEv -> Ptr Data -> IO Data
peekEmptyEv e _ = return (EmptyEv e)

peekCustomEv :: CustomEv -> Ptr Data -> IO Data
peekCustomEv e = peekBody (CustomEv e)

peekExtEv :: ExtEv -> Ptr Data -> IO Data
peekExtEv e p = do
  len <- #{peekintfield snd_seq_ev_ext_t, len} p
  ptr <- #{peek snd_seq_ev_ext_t, ptr} p
  fmap (ExtEv e) $ B.packCStringLen (ptr, fromIntegral len)


data Data
  = NoteEv NoteEv Note
  | CtrlEv CtrlEv Ctrl
  | QueueEv QueueEv Queue.T
  | AddrEv AddrEv Addr.T
  | ConnEv ConnEv Connect.T
  | EmptyEv EmptyEv
  | CustomEv CustomEv Custom
  | ExtEv ExtEv B.ByteString
    deriving Show
