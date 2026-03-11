{-|
  Module      : Bindings.Netcode.IO
  Description : Low-level bindings to the netcode.io library.
  Copyright   : (c) Pavel Krajcevski, 2020
  License     : BSD-3
  Maintainer  : krajcevski@gmail.com
  Stability   : experimental
  Portability : Portable

  This module contains the low-level bindings that represent the direct
  interface between Haskell and the C library
  <https://github.com/networkprotocol/netcode.io netcode.io>.

  The bindings here are meant for advanced usage, as they are not particularly
  idiomatic Haskell, and largely represent the types that we get from the
  <https://hackage.haskell.org/package/bindings-DSL bindings-DSL> library. For
  high level bindings (recommended), please refer to the "Netcode.IO" module.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------

#include <netcode.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Netcode.IO where

import Data.Word              (Word8, Word16, Word64)
import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..), CDouble(..))
import Foreign.Marshal.Array  (peekArray, pokeArray)
import Foreign.Ptr            (Ptr, FunPtr, plusPtr)
import Foreign.Storable       (Storable(..))
import Prelude                ( IO, Eq, Show, Num
                              , ($)
                              , div, undefined, return, take
                              )

--------------------------------------------------------------------------------

#num NETCODE_CONNECT_TOKEN_BYTES
#num NETCODE_KEY_BYTES
#num NETCODE_MAC_BYTES
#num NETCODE_USER_DATA_BYTES
#num NETCODE_MAX_SERVERS_PER_CONNECT

#num NETCODE_CLIENT_STATE_CONNECT_TOKEN_EXPIRED
#num NETCODE_CLIENT_STATE_INVALID_CONNECT_TOKEN
#num NETCODE_CLIENT_STATE_CONNECTION_TIMED_OUT
#num NETCODE_CLIENT_STATE_CONNECTION_RESPONSE_TIMED_OUT
#num NETCODE_CLIENT_STATE_CONNECTION_REQUEST_TIMED_OUT
#num NETCODE_CLIENT_STATE_CONNECTION_DENIED
#num NETCODE_CLIENT_STATE_DISCONNECTED
#num NETCODE_CLIENT_STATE_SENDING_CONNECTION_REQUEST
#num NETCODE_CLIENT_STATE_SENDING_CONNECTION_RESPONSE
#num NETCODE_CLIENT_STATE_CONNECTED

#num NETCODE_MAX_CLIENTS
#num NETCODE_MAX_PACKET_SIZE

#num NETCODE_LOG_LEVEL_NONE
#num NETCODE_LOG_LEVEL_ERROR
#num NETCODE_LOG_LEVEL_INFO
#num NETCODE_LOG_LEVEL_DEBUG

#num NETCODE_OK
#num NETCODE_ERROR

#num NETCODE_ADDRESS_NONE
#num NETCODE_ADDRESS_IPV4
#num NETCODE_ADDRESS_IPV6

#ccall netcode_init, IO CInt
#ccall netcode_term, IO ()

#starttype struct netcode_address_t
#array_field data.ipv4, Word8
#array_field data.ipv6, Word16
#field       port,      Word16
#field       type,      Word8
#stoptype

#ccall netcode_parse_address, CString -> Ptr <netcode_address_t> -> IO CInt
#ccall netcode_address_to_string, Ptr <netcode_address_t> -> CString -> IO CString
#ccall netcode_address_equal, Ptr <netcode_address_t> -> Ptr <netcode_address_t> -> IO CInt

#opaque_t netcode_network_simulator_t
#opaque_t netcode_client_t
#opaque_t netcode_server_t

#callback_t allocate_function_t, Ptr () -> Word64 -> IO (Ptr ())
#callback_t free_function_t, Ptr () -> Ptr () -> IO ()
#callback_t state_change_callback_t, Ptr () -> CInt -> CInt -> IO ()
#callback_t connect_disconnect_callback_t, Ptr () -> CInt -> CInt -> IO ()
#callback_t send_loopback_packet_callback_t, Ptr () -> CInt -> Ptr Word8 -> CInt -> Word64 -> IO ()
#callback_t send_packet_override_t, Ptr () -> Ptr <netcode_address_t> -> Ptr Word8 -> CInt -> IO ()
#callback_t receive_packet_override_t, Ptr () -> Ptr <netcode_address_t> -> Ptr Word8 -> CInt -> IO CInt

#starttype struct netcode_client_config_t
#field allocator_context,             Ptr ()
#field allocate_function,             <allocate_function_t>
#field free_function,                 <free_function_t>
#field network_simulator,             Ptr <netcode_network_simulator_t>
#field callback_context,              Ptr ()
#field state_change_callback,         <state_change_callback_t>
#field send_loopback_packet_callback, <send_loopback_packet_callback_t>
#field override_send_and_receive,     CInt
#field send_packet_override,          <send_packet_override_t>
#field receive_packet_override,       <receive_packet_override_t>
#stoptype

#ccall netcode_default_client_config, Ptr <netcode_client_config_t> -> IO ()
#ccall netcode_client_create, CString -> Ptr <netcode_client_config_t> -> CDouble -> IO (Ptr <netcode_client_t>)
#ccall netcode_client_destroy, Ptr <netcode_client_t> -> IO ()
#ccall netcode_client_connect, Ptr <netcode_client_t> -> Ptr Word8 -> IO ()
#ccall netcode_client_update, Ptr <netcode_client_t> -> CDouble -> IO ()
#ccall netcode_client_next_packet_sequence, Ptr <netcode_client_t> -> IO Word64
#ccall netcode_client_send_packet, Ptr <netcode_client_t> -> Ptr Word8 -> CInt -> IO ()
#ccall netcode_client_receive_packet, Ptr <netcode_client_t> -> Ptr CInt -> Ptr Word64 -> IO (Ptr Word8)
#ccall netcode_client_free_packet, Ptr <netcode_client_t> -> Ptr () -> IO ()
#ccall netcode_client_disconnect, Ptr <netcode_client_t> -> IO ()
#ccall netcode_client_state, Ptr <netcode_client_t> -> IO CInt
#ccall netcode_client_index, Ptr <netcode_client_t> -> IO CInt
#ccall netcode_client_max_clients, Ptr <netcode_client_t> -> IO CInt
#ccall netcode_client_connect_loopback, Ptr <netcode_client_t> -> CInt -> CInt -> IO ()
#ccall netcode_client_disconnect_loopback, Ptr <netcode_client_t> -> IO ()
#ccall netcode_client_process_packet, Ptr <netcode_client_t> -> Ptr <netcode_address_t> -> Ptr Word8 -> CInt -> IO ()
#ccall netcode_client_loopback, Ptr <netcode_client_t> -> IO CInt
#ccall netcode_client_process_loopback_packet, Ptr <netcode_client_t> -> Ptr Word8 -> CInt -> Word64 -> IO ()
#ccall netcode_client_get_port, Ptr <netcode_client_t> -> IO Word16
#ccall netcode_client_server_address, Ptr <netcode_client_t> -> IO (Ptr <netcode_address_t>)
#ccall netcode_generate_connect_token, CInt -> Ptr CString -> Ptr CString -> CInt -> CInt -> Word64 -> Word64 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

#starttype struct netcode_server_config_t
#field       protocol_id,                   Word64
#array_field private_key,                   Word8
#field       allocator_context,             Ptr ()
#field       allocate_function,             <allocate_function_t>
#field       free_function,                 <free_function_t>
#field       network_simulator,             Ptr <netcode_network_simulator_t>
#field       callback_context,              Ptr ()
#field       connect_disconnect_callback,   <connect_disconnect_callback_t>
#field       send_loopback_packet_callback, <send_loopback_packet_callback_t>
#field       override_send_and_receive,     CInt
#field       send_packet_override,          <send_packet_override_t>
#field       receive_packet_override,       <receive_packet_override_t>
#stoptype

#ccall netcode_default_server_config, Ptr <netcode_server_config_t> -> IO ()
#ccall netcode_server_create, CString -> Ptr <netcode_server_config_t> -> CDouble -> IO (Ptr <netcode_server_t>)
#ccall netcode_server_destroy, Ptr <netcode_server_t> -> IO ()
#ccall netcode_server_start, Ptr <netcode_server_t> -> CInt -> IO ()
#ccall netcode_server_stop, Ptr <netcode_server_t> -> IO ()
#ccall netcode_server_running, Ptr <netcode_server_t> -> IO CInt
#ccall netcode_server_max_clients, Ptr <netcode_server_t> -> IO CInt
#ccall netcode_server_update, Ptr <netcode_server_t> -> CDouble -> IO ()
#ccall netcode_server_client_connected, Ptr <netcode_server_t> -> CInt -> IO CInt
#ccall netcode_server_client_id, Ptr <netcode_server_t> -> CInt -> IO Word64
#ccall netcode_server_client_address, Ptr <netcode_server_t> -> CInt -> IO (Ptr <netcode_address_t>)
#ccall netcode_server_disconnect_client, Ptr <netcode_server_t> -> CInt -> IO ()
#ccall netcode_server_disconnect_all_clients, Ptr <netcode_server_t> -> IO ()
#ccall netcode_server_next_packet_sequence, Ptr <netcode_server_t> -> CInt -> IO Word64
#ccall netcode_server_send_packet, Ptr <netcode_server_t> -> CInt -> Ptr Word8 -> CInt -> IO ()
#ccall netcode_server_receive_packet, Ptr <netcode_server_t> -> CInt -> Ptr CInt -> Ptr Word64 -> IO (Ptr Word8)
#ccall netcode_server_free_packet, Ptr <netcode_server_t> -> Ptr () -> IO ()
#ccall netcode_server_num_connected_clients, Ptr <netcode_server_t> -> IO CInt
#ccall netcode_server_client_user_data, Ptr <netcode_server_t> -> CInt -> IO (Ptr ())
#ccall netcode_server_process_packet, Ptr <netcode_server_t> -> Ptr <netcode_address_t> -> Ptr Word8 -> CInt -> IO ()
#ccall netcode_server_connect_loopback_client, Ptr <netcode_server_t> -> CInt -> Word64 -> Ptr Word8 -> IO ()
#ccall netcode_server_disconnect_loopback_client, Ptr <netcode_server_t> -> CInt -> IO ()
#ccall netcode_server_client_loopback, Ptr <netcode_server_t> -> CInt -> IO CInt
#ccall netcode_server_process_loopback_packet, Ptr <netcode_server_t> -> CInt -> Ptr Word8 -> CInt -> Word64 -> IO ()
#ccall netcode_server_get_port, Ptr <netcode_server_t> -> IO Word16

#ccall netcode_log_level, CInt -> IO ()

#callback_t netcode_assert_function_ty, CString -> CString -> CString -> CInt -> IO ()
#ccall netcode_set_assert_function, <netcode_assert_function_ty> -> IO ()

-- void netcode_set_printf_function( int (*function)( NETCODE_CONST char *, ... ) );

#ccall netcode_random_bytes, Ptr Word8 -> CInt -> IO ()
#ccall netcode_sleep, CDouble -> IO ()
#ccall netcode_time, IO CDouble

-- | For testing only.
#ccall netcode_test, IO ()
