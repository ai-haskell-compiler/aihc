-- -*-haskell-*-
{-# LANGUAGE ForeignFunctionInterface #-}

{-
    JACK bindings for Haskell
    Copyright (C) 2011 Henning Thielemann
    Copyright (C) 2007 Soenke Hahn

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

module Sound.JACK.FFI where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (CString)
import Foreign.Storable (Storable, pokeByteOff, peekByteOff)
import qualified Foreign.Storable as St
import qualified Foreign.C.Error as E
import qualified Foreign.C.Types as C

import Control.Applicative (liftA2)

import qualified Data.EnumBitSet as ES
import Data.Word (Word, Word32, Word64, )
import Data.Ix (Ix(range, inRange, rangeSize, index))
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

import qualified Numeric.NonNegative.Class as NonNeg

#include "jack/types.h"
#include <Sound/JACK/Common.h>


data Client = Client
data Port typ = Port

-- | Type argument for Jack input ports
data Input = Input

-- | Type argument for Jack output ports
data Output = Output


foreign import ccall "static jack/jack.h jack_client_open"
  client_open ::
        CString -> OpenOptionSet -> Ptr StatusSet -> CString -> IO (Ptr Client)

{-# DEPRECATED client_new "use client_open instead" #-}
foreign import ccall "static jack/jack.h jack_client_new"
  client_new :: CString -> IO (Ptr Client)

foreign import ccall "static jack/jack.h jack_get_sample_rate"
  get_sample_rate :: Ptr Client -> IO C.CInt


type OpenOptionSet = ES.T C.CULong OpenOptions

data OpenOptions =
     NoStartServer
   | UseExactName
   | ServerName
     deriving (Enum, Eq, Ord, Show, Ix)

wordNullOption, wordNoStartServer, wordUseExactName, wordServerName ::
    OpenOptionSet
wordNullOption    = ES.empty
wordNoStartServer = ES.fromEnum NoStartServer
wordUseExactName  = ES.fromEnum UseExactName
wordServerName    = ES.fromEnum ServerName


type StatusSet = ES.T C.CULong Status

data Status =
     Failure
   | InvalidOption
   | NameNotUnique
   | ServerStarted
   | ServerFailed
   | ServerError
   | NoSuchClient
   | LoadFailure
   | InitFailure
   | ShmFailure
   | VersionError
     deriving (Enum, Eq, Ord, Show, Ix)

wordFailure, wordInvalidOption, wordNameNotUnique, wordServerStarted,
  wordServerFailed, wordServerError, wordNoSuchClient, wordLoadFailure,
  wordInitFailure, wordShmFailure, wordVersionError :: StatusSet
wordFailure       = ES.fromEnum Failure
wordInvalidOption = ES.fromEnum InvalidOption
wordNameNotUnique = ES.fromEnum NameNotUnique
wordServerStarted = ES.fromEnum ServerStarted
wordServerFailed  = ES.fromEnum ServerFailed
wordServerError   = ES.fromEnum ServerError
wordNoSuchClient  = ES.fromEnum NoSuchClient
wordLoadFailure   = ES.fromEnum LoadFailure
wordInitFailure   = ES.fromEnum InitFailure
wordShmFailure    = ES.fromEnum ShmFailure
wordVersionError  = ES.fromEnum VersionError


type PortFlagSet = ES.T C.CULong PortFlag

data PortFlag =
     PortIsInput
   | PortIsOutput
   | PortIsPhysical
   | PortCanMonitor
   | PortIsTerminal
     deriving (Enum, Eq, Ord, Show, Ix)


newtype PortName = PortName {deconsPortName :: CString}

newtype PortType = PortType {deconsPortType :: CString}

portIsInput, portIsOutput :: PortFlagSet
portIsInput  = ES.fromEnum PortIsInput
portIsOutput = ES.fromEnum PortIsOutput


foreign import ccall "static jack/jack.h jack_port_register"
  port_register :: Ptr Client -> PortName -> PortType ->
        PortFlagSet -> C.CULong -> IO (Ptr (Port a))


-- | represents absolute frame time
newtype NFrames = NFrames #{type jack_nframes_t}
    deriving (Show, Eq, Ord)

instance Storable NFrames where
    alignment _ = #{alignment jack_nframes_t}
    sizeOf _ =  #{size jack_nframes_t}
    peek p = fmap NFrames $ peekByteOff p 0
    poke p (NFrames n) = pokeByteOff p 0 n

nframesToWord :: NFrames -> Word
nframesToWord (NFrames n) = fromIntegral n

instance Ix NFrames where
    range (a,b) =
        map (NFrames . fromIntegral) $
        range (nframesToWord a, nframesToWord b)
    index (a,b) i =
        index (nframesToWord a, nframesToWord b) (nframesToWord i)
    inRange (a,b) i =
        inRange (nframesToWord a, nframesToWord b) (nframesToWord i)
    rangeSize (a,b) =
        rangeSize (nframesToWord a, nframesToWord b)

instance Semigroup NFrames where
    NFrames x <> NFrames y = NFrames (x+y)

instance Monoid NFrames where
    mempty = NFrames 0
    mappend = (<>)

instance NonNeg.C NFrames where
    split = NonNeg.splitDefault (\(NFrames n) -> n) NFrames


nframesIndices :: NFrames -> [NFrames]
nframesIndices (NFrames n) =
    take (fromIntegral n) $ map NFrames $ iterate (1+) 0

nframesBounds :: NFrames -> (NFrames,NFrames)
nframesBounds (NFrames n) =
    (NFrames 0, NFrames $ n - 1)


type Process arg = NFrames -> Ptr arg -> IO E.Errno

foreign import ccall "static jack/jack.h jack_set_process_callback"
  set_process_callback ::
        Ptr Client -> FunPtr (Process arg) -> Ptr arg -> IO E.Errno

type Freewheel arg = C.CInt -> Ptr arg -> IO ()

foreign import ccall "static jack/jack.h jack_set_freewheel_callback"
  set_freewheel_callback ::
        Ptr Client -> FunPtr (Freewheel arg) -> Ptr arg -> IO E.Errno

type BufferSize arg = NFrames -> Ptr arg -> IO E.Errno

foreign import ccall "static jack/jack.h jack_set_buffer_size_callback"
  set_buffer_size_callback ::
        Ptr Client -> FunPtr (BufferSize arg) -> Ptr arg -> IO E.Errno

type SampleRate arg = NFrames -> Ptr arg -> IO E.Errno

foreign import ccall "static jack/jack.h jack_set_sample_rate_callback"
  set_sample_rate_callback ::
        Ptr Client -> FunPtr (SampleRate arg) -> Ptr arg -> IO E.Errno

type ClientRegistration arg = CString -> C.CInt -> Ptr arg -> IO ()

foreign import ccall "static jack/jack.h jack_set_client_registration_callback"
  set_client_registration_callback ::
        Ptr Client -> FunPtr (ClientRegistration arg) -> Ptr arg -> IO E.Errno

newtype PortId = PortId #{type jack_port_id_t} deriving (Eq, Ord, Show)

{- |
Type argument for Jack ports where the type of samples transported by the port
is unknown.
-}
data UnknownType = UnknownType

type PortRegistration arg = PortId -> C.CInt -> Ptr arg -> IO ()

foreign import ccall "static jack/jack.h jack_set_port_registration_callback"
  set_port_registration_callback ::
        Ptr Client -> FunPtr (PortRegistration arg) -> Ptr arg -> IO E.Errno

type PortRename arg = PortId -> PortName -> PortName -> Ptr arg -> IO ()

foreign import ccall "static jack/jack.h jack_set_port_rename_callback"
  set_port_rename_callback ::
        Ptr Client -> FunPtr (PortRename arg) -> Ptr arg -> IO E.Errno

type PortConnect arg = PortId -> PortId -> C.CInt -> Ptr arg -> IO ()

foreign import ccall "static jack/jack.h jack_set_port_connect_callback"
  set_port_connect_callback ::
        Ptr Client -> FunPtr (PortConnect arg) -> Ptr arg -> IO E.Errno

type GraphOrder arg = Ptr arg -> IO E.Errno

foreign import ccall "static jack/jack.h jack_set_graph_order_callback"
  set_graph_order_callback ::
        Ptr Client -> FunPtr (GraphOrder arg) -> Ptr arg -> IO E.Errno

type XRun arg = Ptr arg -> IO E.Errno

foreign import ccall "static jack/jack.h jack_set_xrun_callback"
  set_xrun_callback ::
        Ptr Client -> FunPtr (XRun arg) -> Ptr arg -> IO E.Errno

newtype
    LatencyCallbackMode =
        LatencyCallbackMode #{type jack_latency_callback_mode_t}
    deriving (Eq, Ord, Show)

jackCaptureLatency :: LatencyCallbackMode
jackCaptureLatency = LatencyCallbackMode #const JackCaptureLatency

jackPlaybackLatency :: LatencyCallbackMode
jackPlaybackLatency = LatencyCallbackMode #const JackPlaybackLatency

type Latency arg = LatencyCallbackMode -> Ptr arg -> IO ()

foreign import ccall "static jack/jack.h jack_set_latency_callback"
  set_latency_callback ::
        Ptr Client -> FunPtr (Latency arg) -> Ptr arg -> IO E.Errno

foreign import ccall "static jack/jack.h jack_port_by_id"
  port_by_id ::
        Ptr Client -> PortId -> IO (Ptr (Port UnknownType))

foreign import ccall "static jack/jack.h jack_port_by_name"
  port_by_name ::
        Ptr Client -> PortName -> IO (Ptr (Port UnknownType))

foreign import ccall "static jack/jack.h jack_port_name"
  port_name ::
        Ptr (Port typ) -> IO PortName

foreign import ccall "static jack/jack.h jack_port_name_size"
  port_name_size ::
        IO C.CInt

foreign import ccall "static jack/jack.h jack_port_get_aliases"
  port_get_aliases ::
        Ptr (Port typ) -> Ptr CString -> IO C.CInt

foreign import ccall "static jack/jack.h jack_port_short_name"
  port_short_name ::
        Ptr (Port typ) -> IO CString

foreign import ccall "static jack/jack.h jack_port_flags"
  port_flags ::
        Ptr (Port typ) -> IO PortFlagSet

foreign import ccall "static jack/jack.h jack_port_type"
  port_type ::
        Ptr (Port UnknownType) -> IO PortType

foreign import ccall "static jack/jack.h jack_get_ports"
  get_ports :: Ptr Client -> CString -> CString -> C.CULong -> IO (Ptr CString)
--  get_ports :: Ptr Client -> CString -> CString -> C.CULong -> IO (Ptr PortName)

foreign import ccall "static jack/jack.h jack_port_get_all_connections"
  port_get_all_connections ::
        Ptr Client -> Ptr (Port typ) -> IO (Ptr CString)

data LatencyRange = LatencyRange NFrames NFrames deriving (Show, Eq, Ord)

instance Storable LatencyRange where
    alignment _ = #{alignment jack_latency_range_t}
    sizeOf _ =  #{size jack_latency_range_t}
    peek p =
        liftA2 LatencyRange
            (#{peek jack_latency_range_t, min} p)
            (#{peek jack_latency_range_t, max} p)
    poke p (LatencyRange a b) = do
        #{poke jack_latency_range_t, min} p a
        #{poke jack_latency_range_t, max} p b

foreign import ccall "static jack/jack.h jack_port_get_latency_range"
  port_get_latency_range ::
        Ptr (Port a) -> LatencyCallbackMode -> Ptr LatencyRange -> IO ()

foreign import ccall "static jack/jack.h jack_port_set_latency_range"
  port_set_latency_range ::
        Ptr (Port a) -> LatencyCallbackMode -> Ptr LatencyRange -> IO ()

foreign import ccall "static jack/jack.h jack_recompute_total_latencies"
  recompute_total_latencies ::
        Ptr Client -> IO E.Errno

foreign import ccall "static jack/jack.h jack_last_frame_time"
  last_frame_time :: Ptr Client -> IO NFrames

foreign import ccall "static jack/jack.h jack_port_get_buffer"
  port_get_buffer :: Ptr (Port a) -> NFrames -> IO (Ptr a)

foreign import ccall "static jack/jack.h jack_get_buffer_size"
  get_buffer_size :: Ptr Client -> IO (C.CUInt)

foreign import ccall "static jack/jack.h jack_activate"
  activate :: Ptr Client -> IO E.Errno

foreign import ccall "static jack/jack.h jack_client_close"
  client_close :: Ptr Client -> IO E.Errno

-- may return eEXIST
foreign import ccall "static jack/jack.h jack_connect"
  connect :: Ptr Client -> PortName -> PortName -> IO E.Errno

foreign import ccall "static jack/jack.h jack_disconnect"
  disconnect :: Ptr Client -> PortName -> PortName -> IO E.Errno

foreign import ccall "static jack/jack.h jack_port_unregister"
  port_unregister :: Ptr Client -> Ptr (Port a) -> IO E.Errno

foreign import ccall "static jack/jack.h jack_deactivate"
  deactivate :: Ptr Client -> IO E.Errno


_dummy :: (Word32, Word64)
_dummy = undefined
