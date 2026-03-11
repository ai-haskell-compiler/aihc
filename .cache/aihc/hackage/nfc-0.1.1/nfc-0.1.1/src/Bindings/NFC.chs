{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Bindings.NFC 
  ( NFCTarget(..)
  , NFCModulation(..)
  , NFCModulationType(..)
  , NFCBaudRate(..)
  , NFCISO14443aInfo(..)
  , initialize
  , open
  , initiatorInit
  , initiatorSelectPassiveTarget
  , initiatorPollTarget
  ) where

import Control.Monad         ((<=<))
import Data.ByteString       (ByteString, packCStringLen)
import Data.Word             (Word8, Word16)
import Foreign.C.String      (withCStringLen)
import Foreign.C.Types       (CUChar(..))
import Foreign.ForeignPtr    (newForeignPtr, withForeignPtr)
import Foreign.Ptr           (Ptr, castPtr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Marshal.Utils (maybePeek, with)
import Foreign.Storable      (Storable(..))

data NFCModulation =
  NFCModulation { nfcModType     :: NFCModulationType
                , nfcModBaudRate :: NFCBaudRate
                }

data NFCISO14443aInfo =
  NFCISO14443aInfo { iso14443aAbtAtqa :: Word16
                   , iso14443aBtSak   :: Word8
                   , iso14443aAbtUid  :: ByteString
                   , iso14443aAbtAts  :: ByteString
                   }

-- Add additional protocols here
data NFCTarget = NFCTargetISO14443a NFCISO14443aInfo

outNfcCtx :: Ptr (Ptr ()) -> IO NFCContextPtr
outNfcCtx = newForeignPtr nfc_exit <=< peek

outNfcDev :: Ptr () -> IO (Maybe NFCDevicePtr)
outNfcDev = maybePeek $ newForeignPtr nfc_close

inMaybeStrLen :: Num a => Maybe String -> ((Ptr CUChar, a) -> IO b) -> IO b
inMaybeStrLen Nothing    f = f (nullPtr, 0)
inMaybeStrLen (Just str) f = withCStringLen str $ \(ptr, len) -> f (castPtr ptr, fromIntegral len)

inNfcMod :: Num a => [NFCModulation] -> ((Ptr NFCModulation, a) -> IO b) -> IO b
inNfcMod nfcMod f = do
  let len = length nfcMod

  allocaArray len $ \ptr -> do
    pokeArray ptr nfcMod
    f (ptr, fromIntegral len)

#include "nfc/nfc.h"
#include "nfc/nfc-types.h"

#c

nfc_modulation *hs_nfc_get_nm(nfc_target *t);
size_t hs_nfc_target_size();
uint8_t *hs_nfc_get_nai_abtAtqa(nfc_target *t);
uint8_t  hs_nfc_get_nai_btSak(nfc_target *t);
size_t   hs_nfc_get_nai_szUidLen(nfc_target *t);
uint8_t *hs_nfc_get_nai_abtUid(nfc_target *t);
size_t   hs_nfc_get_nai_szAtsLen(nfc_target *t);
uint8_t *hs_nfc_get_nai_abtAts(nfc_target *t);
int __wrapped__nfc_initiator_select_passive_target (nfc_device *,
                                                    const nfc_modulation *,
                                                    const uint8_t *,
                                                    const size_t,
                                                    nfc_target *);

#endc

{#enum nfc_modulation_type as NFCModulationType {underscoreToCase} deriving (Eq,Show)#}

{#enum nfc_baud_rate as NFCBaudRate {underscoreToCase} deriving (Eq, Show)#}

{#pointer *nfc_context as NFCContextPtr foreign finalizer nfc_exit#}

{#pointer *nfc_device as NFCDevicePtr foreign finalizer nfc_close#}

{#pointer *nfc_modulation as NFCModulationPtr foreign -> NFCModulation#}

{#pointer *nfc_target as NFCTargetPtr foreign -> NFCTarget#}

{#fun nfc_init as initialize {alloca- `NFCContextPtr' outNfcCtx*} -> `()'#}

{#fun nfc_open as open {`NFCContextPtr', `String'} -> `Maybe NFCDevicePtr' outNfcDev*#}

{#fun nfc_initiator_init as initiatorInit {`NFCDevicePtr'} -> `Int'#}

initiatorSelectPassiveTarget :: NFCDevicePtr -> NFCModulation -> Maybe String -> IO (Maybe NFCTarget)
initiatorSelectPassiveTarget dev nfcMod initData =
  withForeignPtr dev $ \devPtr ->
    with nfcMod $ \nfcModPtr ->
      inMaybeStrLen initData $ \(strPtr, strLen) ->
        alloca $ \target -> do
          returnValue <- {#call __wrapped__nfc_initiator_select_passive_target#}
                         devPtr
                         nfcModPtr
                         strPtr
                         strLen
                         target

          if returnValue /= 1
            then return Nothing
            else (return . Just <=< peek) target

initiatorPollTarget :: NFCDevicePtr -> [NFCModulation] -> Word8 -> Word8 -> IO (Maybe NFCTarget)
initiatorPollTarget dev nfcMod numPolling period = do
  withForeignPtr dev $ \devPtr ->
    inNfcMod nfcMod $ \(nfcModArray, nfcModLen) ->
      alloca $ \target -> do
        returnValue <- {#call nfc_initiator_poll_target#}
                       devPtr
                       nfcModArray
                       nfcModLen
                       (fromIntegral numPolling)
                       (fromIntegral period)
                       target

        if returnValue /= 1
          then return Nothing
          else (return . Just <=< peek) target

decodeIso14443a :: Ptr NFCTarget -> IO NFCTarget
decodeIso14443a p = do
  abtAtqa        <- (peek . castPtr) =<< {#call hs_nfc_get_nai_abtAtqa#} p

  (CUChar btSak) <- {#call hs_nfc_get_nai_btSak#} p

  szUidLen       <- fromIntegral <$> {#call hs_nfc_get_nai_szUidLen#} p
  abtUidPtr      <- castPtr      <$> {#call hs_nfc_get_nai_abtUid#} p
  abtUid         <- packCStringLen (abtUidPtr, szUidLen)

  szAtsLen       <- fromIntegral <$> {#call hs_nfc_get_nai_szAtsLen#} p
  abtAtsPtr      <- castPtr      <$> {#call hs_nfc_get_nai_abtAts#} p
  abtAts         <- packCStringLen (abtAtsPtr, szAtsLen)

  return $ NFCTargetISO14443a $ NFCISO14443aInfo abtAtqa btSak abtUid abtAts

instance Storable NFCModulation where
  alignment _ = {#alignof nfc_modulation#}
  sizeOf    _ = {#sizeof nfc_modulation#}
  peek p      = do
    modType <- (toEnum . fromIntegral) <$> {#get nfc_modulation.nmt#} p
    baud    <- (toEnum . fromIntegral) <$> {#get nfc_modulation.nbr#} p
    return $ NFCModulation modType baud
  poke p (NFCModulation modType baud) = do
    {#set nfc_modulation.nmt#} p $ (fromIntegral . fromEnum) modType
    {#set nfc_modulation.nbr#} p $ (fromIntegral . fromEnum) baud

instance Storable NFCTarget where
  alignment _ = {#alignof nfc_target#}
  sizeOf    _ = fromIntegral {#call pure hs_nfc_target_size#}
  peek p      = do
    nfcMod <- peek =<< {#call hs_nfc_get_nm#} p
    case nfcModType nfcMod of
      NmtIso14443a -> decodeIso14443a p
      _            -> error "not implemented"
  poke        = error "not implemented"
