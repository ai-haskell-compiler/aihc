{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, DeriveGeneric #-}

#include "hidapi.h"

module System.HIDAPI
  ( System.HIDAPI.init, exit, withHIDAPI
  , enumerate, enumerateAll
  , open, openPath, openDeviceInfo
  , close
  , System.HIDAPI.read
  , System.HIDAPI.readTimeout
  , System.HIDAPI.setBlocking
  , System.HIDAPI.write
  , System.HIDAPI.getFeatureReport
  , System.HIDAPI.sendFeatureReport
  , getSerialNumberString
  , System.HIDAPI.error
  , HIDAPIException(HIDAPIException)
  , Device()
  , DeviceInfo (..)
  , DevicePath
  , VendorID
  , ProductID
  , ReleaseNumber
  , SerialNumber
  , InterfaceNumber
  ) where

import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Exception
import Control.Monad
import Data.ByteString
import Data.ByteString.Internal (createAndTrim)
import Data.Maybe
import Data.Typeable
import Data.Data
import Foreign
import Foreign.C.Types
import Foreign.C.String
import GHC.Generics (Generic)

type Hid_Device_Ptr = Ptr () -- used where hid_device* is used on C side
newtype Device = Device Hid_Device_Ptr

data DeviceInfoInternal = DeviceInfoInternal
  { _path :: CString
  , _vendorId :: CUShort
  , _productId :: CUShort
  , _serialNumber :: CWString
  , _releaseNumber :: CUShort
  , _manufacturerString :: CWString
  , _productString :: CWString
  , _usagePage :: CUShort
  , _usage :: CUShort
  , _interfaceNumber :: CInt
  , _next :: Ptr DeviceInfoInternal
  } deriving (Show)

-- | Note: This is currently a read-only instance. `poke` is not yet implemented.
peekDeviceInfoInternal :: Ptr DeviceInfoInternal -> IO DeviceInfoInternal
peekDeviceInfoInternal p =
  DeviceInfoInternal <$>
    #{peek struct hid_device_info, path} p <*>
    #{peek struct hid_device_info, vendor_id} p <*>
    #{peek struct hid_device_info, product_id} p <*>
    #{peek struct hid_device_info, serial_number} p <*>
    #{peek struct hid_device_info, release_number} p <*>
    #{peek struct hid_device_info, manufacturer_string} p <*>
    #{peek struct hid_device_info, product_string} p <*>
    #{peek struct hid_device_info, usage_page} p <*>
    #{peek struct hid_device_info, usage} p <*>
    #{peek struct hid_device_info, interface_number} p <*>
    #{peek struct hid_device_info, next} p

-- Aliases for writing better type signatures and haddocks.
type DevicePath = String
type VendorID = Word16
type ProductID = Word16
type ReleaseNumber = Word16
type SerialNumber = String
type InterfaceNumber = Int
type ReportID = Word8
type ReportLength = Word16
type FeatureReport = (ReportID, ByteString)

data DeviceInfo = DeviceInfo
  { path :: DevicePath
  , vendorId :: VendorID
  , productId :: ProductID
  , serialNumber :: Maybe SerialNumber
  , releaseNumber :: ReleaseNumber
  , manufacturerString :: Maybe String
  , productString :: Maybe String
  , usagePage :: Word16
  , usage :: Word16
  , interfaceNumber :: InterfaceNumber
  } deriving (Show, Generic)

instance NFData DeviceInfo where rnf = genericRnf

peekOptString :: Ptr CWchar -> IO (Maybe String)
peekOptString p
  | p == nullPtr = return Nothing
  | otherwise = Just <$> peekCWString p

fromInternalDeviceInfo :: DeviceInfoInternal -> IO DeviceInfo
fromInternalDeviceInfo di = DeviceInfo <$>
  peekCString (_path di) <*>
  pure (fromIntegral $ _vendorId di) <*>
  pure (fromIntegral $ _productId di) <*>
  peekOptString (_serialNumber di) <*>
  pure (fromIntegral $ _releaseNumber di) <*>
  peekOptString (_manufacturerString di) <*>
  peekOptString (_productString di) <*>
  pure (fromIntegral $ _usagePage di) <*>
  pure (fromIntegral $ _usage di) <*>
  pure (fromIntegral $ _interfaceNumber di)

foreign import ccall safe "hidapi/hidapi.h hid_error"
  hid_error :: Hid_Device_Ptr -> IO CWString

data HIDAPIException = HIDAPIException String String
  deriving (Data, Typeable, Generic)

instance Show HIDAPIException where
  showsPrec _ (HIDAPIException a c) = showString a . showString ": " . showString c

instance Exception HIDAPIException

instance NFData HIDAPIException where rnf = genericRnf

-- TODO As of https://github.com/signal11/hidapi/issues/123 it is unclear
--      whether the passed in pointer may be NULL, and if hid_error ever
--      returns something useful.
error :: Device -> IO (Maybe String)
error (Device devicePtr) = do
  e <- hid_error devicePtr
  if e == nullPtr
    then return Nothing
    else do
      es <- peekCWString e
      free e
      return (Just es)

check :: Bool -> String -> String -> IO ()
check c msg reason = unless c $ throwIO $ HIDAPIException msg reason

-- Device is only used to obtain a more detailed error if the condition is
-- false and device is not a NULL pointer.
checkWithHidError :: Bool -> Device -> String -> String -> IO ()
checkWithHidError c dev@(Device devPtr) msg defaultReason = unless c $ do
  reason <- if devPtr /= nullPtr
              then fromMaybe defaultReason <$> System.HIDAPI.error dev
              else return defaultReason
  throwIO $ HIDAPIException msg reason

foreign import ccall safe "hidapi/hidapi.h hid_init"
  hid_init :: IO CInt

init :: IO ()
init = do
  r <- hid_init
  check (r == 0) "HIDAPI initialization failed" "hid_init /= 0"

foreign import ccall safe "hidapi/hidapi.h hid_exit"
  hid_exit :: IO CInt

exit :: IO ()
exit = do
  r <- hid_exit
  check (r == 0) "HIDAPI shutdown failed" "hid_exit /= 0"

withHIDAPI :: IO a -> IO a
withHIDAPI = bracket_ System.HIDAPI.init exit

foreign import ccall safe "hidapi/hidapi.h hid_enumerate"
  hid_enumerate :: CUShort -> CUShort -> IO (Ptr DeviceInfoInternal)

foreign import ccall safe "hidapi/hidapi.h hid_free_enumeration"
  hid_free_enumeration :: Ptr DeviceInfoInternal -> IO ()

parseEnumeration :: Ptr DeviceInfoInternal -> IO [ DeviceInfo ]
parseEnumeration dip
  | dip == nullPtr = return []
  | otherwise = do
    idi <- peekDeviceInfoInternal dip
    di <- fromInternalDeviceInfo idi
    dis <- parseEnumeration (_next idi)
    return (di : dis)

enumerate :: Maybe VendorID -> Maybe ProductID -> IO [ DeviceInfo ]
enumerate m'vendorId m'productId = do
  dip <- hid_enumerate (maybe 0 fromIntegral m'vendorId) (maybe 0 fromIntegral m'productId)
  -- Docs say hid_enumerate also returns "NULL in the case of failure", but
  -- this is indistinguishable from "no devices". :/
  if dip == nullPtr
    then return []
    else do
      dis <- parseEnumeration dip
      hid_free_enumeration dip
      return dis

enumerateAll :: IO [ DeviceInfo ]
enumerateAll = enumerate Nothing Nothing

foreign import ccall safe "hidapi/hidapi.h hid_open"
  hid_open :: CUShort -> CUShort -> CWString -> IO Hid_Device_Ptr

open :: VendorID -> ProductID -> Maybe SerialNumber -> IO Device
open vendor_id product_id serial = do
  let vid = fromIntegral vendor_id
  let pid = fromIntegral product_id
  dev@(Device dp) <- Device <$> case serial of
    Nothing -> hid_open vid pid nullPtr
    Just sn -> withCWString sn (hid_open vid pid)
  checkWithHidError (dp /= nullPtr) dev "Device open (by vendor/product id) failed" "hid_open returned NULL"
  return dev

foreign import ccall safe "hidapi/hidapi.h hid_open_path"
  hid_open_path :: CString -> IO Hid_Device_Ptr

openPath :: DevicePath -> IO Device
openPath p = do
  dev@(Device dp) <- Device <$> withCString p hid_open_path
  checkWithHidError (dp /= nullPtr) dev "Device open (by path) failed" "hid_open returned NULL"
  return dev

openDeviceInfo :: DeviceInfo -> IO Device
openDeviceInfo
  = openPath . path

foreign import ccall safe "hidapi/hidapi.h hid_close"
  close :: Device -> IO ()

foreign import ccall safe "hidapi/hidapi.h hid_read"
  hid_read :: Device -> Ptr CChar -> CSize -> IO CInt

foreign import ccall safe "hidapi/hidapi.h hid_read_timeout"
  hid_read_timeout :: Device -> Ptr CChar -> CSize -> CInt -> IO CInt

foreign import ccall safe "hidapi/hidapi.h hid_write"
  hid_write :: Device -> Ptr CChar -> CSize -> IO CInt

foreign import ccall safe "hidapi/hidapi.h hid_get_feature_report"
  hid_get_feature_report :: Device -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall safe "hidapi/hidapi.h hid_send_feature_report"
  hid_send_feature_report :: Device -> Ptr CChar -> CSize -> IO CInt

foreign import ccall safe "hidapi/hidapi.h hid_set_nonblocking"
  hid_set_nonblocking :: Device -> CInt -> IO CInt

setBlocking :: Device -- USB Device to act on, see open, openPath or openDeviceInfo
            -> Bool   -- True -> read blocks; False -> read may return immediately with 0 bytes read
            -> IO ()
setBlocking dev blocking = do
  n' <- hid_set_nonblocking dev $ if blocking then 0 else 1
  checkWithHidError (n' /= -1) dev "Unable to set blocking mode" "hid_set_nonblocking returned -1"

read :: Device -> Int -> IO ByteString
read dev n = allocaBytes n $ \b -> do
  n' <- hid_read dev b (fromIntegral n)
  checkWithHidError (n' /= -1) dev "Read failed" "hid_read returned -1"
  packCStringLen ( b, fromIntegral n' )

readTimeout :: Device -> Int -> Int -> IO ByteString
readTimeout dev n time = allocaBytes n $ \b -> do
  n' <- hid_read_timeout dev b (fromIntegral n) (fromIntegral time)
  checkWithHidError (n' /= -1) dev "Read failed" "hid_read_timeout returned -1"
  packCStringLen ( b, fromIntegral n' )

write :: Device -> ByteString -> IO Int
write dev b = do
  n' <- useAsCStringLen b $ \(cs, csLen) -> hid_write dev cs (fromIntegral csLen)
  checkWithHidError (n' /= -1) dev "Write failed" "hid_write returned -1"
  return $ fromIntegral n'

getFeatureReport :: Device -> ReportID -> ReportLength -> IO FeatureReport
getFeatureReport dev r l = do
  b <- createAndTrim (toSize l) $ \cs -> do
    _ <- pokeElemOff cs 0 (fromIntegral r)
    n <- hid_get_feature_report dev cs (toSize l)
    let n' = fromIntegral n
    checkWithHidError (n' /= -1) dev "Write failed" "hid_get_feature_report returned -1"
    return n'
  return (Data.ByteString.head b, Data.ByteString.tail b)
  where
    toSize x = fromIntegral x + 1

sendFeatureReport :: Device -> ReportID -> ByteString -> IO Int
sendFeatureReport dev r d = do
  let b = cons r d
  n' <- useAsCStringLen b $ \(cs, csLen) -> hid_send_feature_report dev cs (fromIntegral csLen)
  checkWithHidError (n' /= -1) dev "Write failed" "hid_send_feature_report returned -1"
  return $ fromIntegral n'

foreign import ccall safe "hidapi/hidapi.h hid_get_serial_number_string"
  hid_get_serial_number_string :: Device -> CWString -> CSize -> IO CInt

_SERIAL_NUMBER_MAX_LENGTH :: Int
_SERIAL_NUMBER_MAX_LENGTH = 32768

getSerialNumberString :: Device -> IO SerialNumber
getSerialNumberString dev = do
  let bs = _SERIAL_NUMBER_MAX_LENGTH * sizeOf (undefined :: CWchar)
  allocaBytes bs $ \b -> do
    n' <- hid_get_serial_number_string dev b (fromIntegral _SERIAL_NUMBER_MAX_LENGTH)
    checkWithHidError (n' /= -1) dev "Getting serial number failed" "hid_get_serial_number_string returned -1"
    peekCWString b
