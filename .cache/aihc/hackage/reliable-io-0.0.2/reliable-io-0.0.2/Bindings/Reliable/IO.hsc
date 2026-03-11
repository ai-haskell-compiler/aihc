{-|
  Module      : Bindings.Reliable.IO
  Description : Low-level bindings to the reliable.io library.
  Copyright   : (c) Pavel Krajcevski, 2020
  License     : BSD-3
  Maintainer  : krajcevski@gmail.com
  Stability   : experimental
  Portability : Portable

  This module contains the low-level bindings that represent the direct
  interface between Haskell and the C library
  <https://github.com/networkprotocol/reliable.io reliable.io>.

  The bindings here are meant for advanced usage, as they are not particularly
  idiomatic Haskell, and largely represent the types that we get from the
  <https://hackage.haskell.org/package/bindings-DSL bindings-DSL> library. For
  high level bindings (recommended), please refer to the "Reliable.IO" module.
-}

{-# LANGUAGE NoImplicitPrelude  #-}
--------------------------------------------------------------------------------

#include <reliable.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Reliable.IO where

import Data.Word              (Word8, Word16, Word64)
import Foreign.C.String       (CString)
import Foreign.C.Types        (CChar(..), CInt(..), CFloat(..), CDouble(..))
import Foreign.Marshal.Array  (peekArray, pokeArray)
import Foreign.Ptr            (Ptr, FunPtr, plusPtr)
import Foreign.Storable       (Storable(..))
import Prelude                ( IO, Eq, Show, Num
                              , ($)
                              , div, undefined, return, take
                              )

--------------------------------------------------------------------------------

#num RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_SENT
#num RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_RECEIVED
#num RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_ACKED
#num RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_STALE
#num RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_INVALID
#num RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_TOO_LARGE_TO_SEND
#num RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_TOO_LARGE_TO_RECEIVE
#num RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_SENT
#num RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_RECEIVED
#num RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_INVALID
#num RELIABLE_ENDPOINT_NUM_COUNTERS

#num RELIABLE_LOG_LEVEL_NONE
#num RELIABLE_LOG_LEVEL_ERROR
#num RELIABLE_LOG_LEVEL_INFO
#num RELIABLE_LOG_LEVEL_DEBUG

#num RELIABLE_OK
#num RELIABLE_ERROR

#ccall reliable_init, IO CInt
#ccall reliable_term, IO ()

#callback_t allocate_function_t,        Ptr () -> Word64 -> IO (Ptr ())
#callback_t free_function_t,            Ptr () -> Ptr () -> IO ()
#callback_t transmit_packet_function_t, Ptr () -> CInt -> Word16 -> Ptr Word8 -> CInt -> IO ()
#callback_t process_packet_function_t,  Ptr () -> CInt -> Word16 -> Ptr Word8 -> CInt -> IO CInt

#starttype struct reliable_config_t
#array_field name,                            CChar
#field       context,                         Ptr ()
#field       index,                           CInt
#field       max_packet_size,                 CInt
#field       fragment_above,                  CInt
#field       max_fragments,                   CInt
#field       fragment_size,                   CInt
#field       ack_buffer_size,                 CInt
#field       sent_packets_buffer_size,        CInt
#field       received_packets_buffer_size,    CInt
#field       fragment_reassembly_buffer_size, CInt
#field       rtt_smoothing_factor,            CFloat
#field       packet_loss_smoothing_factor,    CFloat
#field       bandwidth_smoothing_factor,      CFloat
#field       packet_header_size,              CInt
#field       transmit_packet_function,        <transmit_packet_function_t>
#field       process_packet_function,         <process_packet_function_t>
#field       allocator_context,               Ptr ()
#field       allocate_function,               <allocate_function_t>
#field       free_function,                   <free_function_t>
#stoptype

#opaque_t reliable_endpoint_t 

#ccall reliable_default_config, Ptr <reliable_config_t> -> IO ()
#ccall reliable_endpoint_create, Ptr <reliable_config_t> -> CDouble -> IO (Ptr <reliable_endpoint_t>)
#ccall reliable_endpoint_next_packet_sequence, Ptr <reliable_endpoint_t> -> IO Word16
#ccall reliable_endpoint_send_packet, Ptr <reliable_endpoint_t> -> Ptr Word8 -> CInt -> IO ()
#ccall reliable_endpoint_receive_packet, Ptr <reliable_endpoint_t> -> Ptr Word8 -> CInt -> IO ()
#ccall reliable_endpoint_get_acks, Ptr <reliable_endpoint_t> -> Ptr CInt -> IO (Ptr Word16)
#ccall reliable_endpoint_clear_acks, Ptr <reliable_endpoint_t> -> IO ()
#ccall reliable_endpoint_reset, Ptr <reliable_endpoint_t> -> IO ()
#ccall reliable_endpoint_update, Ptr <reliable_endpoint_t> -> CDouble -> IO ()
#ccall reliable_endpoint_rtt, Ptr <reliable_endpoint_t> -> IO CFloat
#ccall reliable_endpoint_packet_loss, Ptr <reliable_endpoint_t> -> IO CFloat
#ccall reliable_endpoint_bandwidth, Ptr <reliable_endpoint_t> -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
#ccall reliable_endpoint_counters, Ptr <reliable_endpoint_t> -> IO (Ptr Word64)
#ccall reliable_endpoint_destroy, Ptr <reliable_endpoint_t> -> IO ()
#ccall reliable_log_level, CInt -> IO ()

-- | Generally not useful -- just calls 'free_function' as used in the config.
#ccall reliable_endpoint_free_packet, Ptr <reliable_endpoint_t> -> Ptr () -> IO ()

-- Only available via low level bindings. In order to use these, the cc-flags
-- for this package need to remove NDEBUG as part of the preprocessor options.
#callback_t assert_function_t, CString -> CString -> CString -> CInt -> IO ()
#ccall reliable_set_assert_function, <assert_function_t> -> IO ()

-- | For testing only.
#ccall reliable_test, IO ()
