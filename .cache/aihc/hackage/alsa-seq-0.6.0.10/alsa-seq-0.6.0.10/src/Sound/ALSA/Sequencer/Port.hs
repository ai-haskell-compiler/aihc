--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Port
-- Copyright : (c) Henning Thielemann, 2010
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- This module contains functions for working with ports.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_port.html>
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Port
  ( Port.T(Port.Cons)
  , create
  , createSimple
  , delete
  , deleteSimple

  , withSimple

  , Port.systemTimer
  , Port.systemAnnounce
  , Port.unknown

  , Port.Cap
  , Port.capRead
  , Port.capWrite
  , Port.capSyncRead
  , Port.capSyncWrite
  , Port.capDuplex
  , Port.capSubsRead
  , Port.capSubsWrite
  , Port.capNoExport
  , Port.caps

  , Port.Type
  , Port.typeSpecific
  , Port.typeMidiGeneric
  , Port.typeMidiGM
  , Port.typeMidiGS
  , Port.typeMidiXG
  , Port.typeMidiMT32
  , Port.typeMidiGM2

  , Port.typeSynth
  , Port.typeDirectSample
  , Port.typeSample

  , Port.typeHardware
  , Port.typeSoftware
  , Port.typeSynthesizer
  , Port.typePort
  , Port.typeApplication
  , Port.types

  ) where

import qualified Sound.ALSA.Sequencer.Marshal.Port as Port
import qualified Sound.ALSA.Sequencer.Marshal.PortInfo as PortInfo
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.C.String (CString, withCAString, )
import Foreign.Ptr (Ptr, )

import qualified Data.EnumBitSet as EnumSet -- expose EnumSet.Cons for foreign call

import Control.Exception (bracket, )



--------------------------------------------------------------------------------

-- | Create a port - simple version.
createSimple :: Seq.T mode -> String -> Port.Cap -> Port.Type -> IO Port.T
createSimple (Seq.Cons h) s c t =
  withCAString s $ \a ->
    Port.imp `fmap` (Exc.checkResult "create_simple_port" =<< snd_seq_create_simple_port h a c t)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_create_simple_port"
  snd_seq_create_simple_port :: Ptr Seq.Core -> CString -> Port.Cap -> Port.Type
                                -> IO C.CInt


-- | Delete the port.
deleteSimple :: Seq.T mode -> Port.T -> IO ()
deleteSimple (Seq.Cons h) (Port.Cons p) =
  Exc.checkResult_ "delete_simple_port" =<< snd_seq_delete_simple_port h (fromIntegral p)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_delete_simple_port"
  snd_seq_delete_simple_port :: Ptr Seq.Core -> C.CInt -> IO C.CInt


withSimple ::
   Seq.T mode -> String -> Port.Cap -> Port.Type ->
   (Port.T -> IO a) ->
   IO a
withSimple ss s c t =
   bracket (createSimple ss s c t) (deleteSimple ss)



--------------------------------------------------------------------------------

-- | Create a new port, as described by the info structure.
create :: Seq.T mode -> PortInfo.T -> IO ()
create (Seq.Cons h) p =
  Exc.checkResult_ "create_port" =<< PortInfo.with p (snd_seq_create_port h)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_create_port"
  snd_seq_create_port :: Ptr Seq.Core -> Ptr PortInfo.T_ -> IO C.CInt

-- | Delete the port.
delete :: Seq.T mode -> Port.T -> IO ()
delete (Seq.Cons h) (Port.Cons p) =
  Exc.checkResult_ "delete_port" =<< snd_seq_delete_port h (fromIntegral p)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_delete_port"
  snd_seq_delete_port :: Ptr Seq.Core -> C.CInt -> IO C.CInt



_dummyEnumSet :: EnumSet.T Int Bool
_dummyEnumSet = undefined
