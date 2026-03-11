{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Netcode.IO.Address where

--------------------------------------------------------------------------------

import Data.Data             (Data)
import Data.Typeable         (Typeable)
import Data.Word             (Word8, Word16)
import Control.Monad         (unless)
import Foreign.C.String      (withCString, peekCString)
import Foreign.ForeignPtr    (ForeignPtr, withForeignPtr, mallocForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable      (peek, poke)
import GHC.Generics          (Generic)

import Bindings.Netcode.IO

--------------------------------------------------------------------------------

-- | An opaque type that encapsulates an address that can be used with
-- @netcode.io@. The address may be stored in memory in different ways (for
-- example with encryption), and therefore needs the IO monad to interact with
-- it.
--
-- <https://github.com/Mokosha/netcode-io/issues/1 TODO>: Use a high-level
-- representation here to enable a more pure interface.
newtype Address = Address (ForeignPtr C'netcode_address_t)
                deriving (Show)

-- | The address mode based on how the address is represented. Usually a
-- consequence of how it was parsed.
data AddressMode
  = AddressMode'Unknown  -- ^ Usually when address is stored encrypted.
  | AddressMode'IPv4
  | AddressMode'IPv6
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable, Generic)

typedAddressMode :: Word8 -> AddressMode
typedAddressMode m
  | m == c'NETCODE_ADDRESS_NONE = AddressMode'Unknown
  | m == c'NETCODE_ADDRESS_IPV4 = AddressMode'IPv4
  | m == c'NETCODE_ADDRESS_IPV6 = AddressMode'IPv6
  | otherwise = error "Unknown address mode!"

rawAddressMode :: AddressMode -> Word8
rawAddressMode AddressMode'Unknown = c'NETCODE_ADDRESS_NONE
rawAddressMode AddressMode'IPv4    = c'NETCODE_ADDRESS_IPV4
rawAddressMode AddressMode'IPv6    = c'NETCODE_ADDRESS_IPV6

-- | Returns the address mode for the given address. Note, this address may be
-- stored in memory in different ways, so we must inspect the memory contents
-- in order to retreive the addressing mode.
addressMode :: Address -> IO AddressMode
addressMode (Address addr) = withForeignPtr addr $
  fmap (typedAddressMode . c'netcode_address_t'type) . peek

-- | Returns the port associated with this address.
addressPort :: Address -> IO Word16
addressPort (Address addr) = withForeignPtr addr $
  fmap c'netcode_address_t'port . peek

-- | Returns the address as a sequence of values in its human readable format.
-- For example:
--
-- >>> parseAddress "123.231.132.213" >>= addressValues
-- [123, 231, 132, 213]
--
-- The length of the list is either 4 or 8 depending on the address mode. If
-- the address mode is unknown, 'addressValues' returns the empty list.
addressValues :: Address -> IO [Word16]
addressValues (Address fptr) = withForeignPtr fptr $ \aptr -> do
  addr <- peek aptr
  case (typedAddressMode $ c'netcode_address_t'type addr) of
    AddressMode'IPv4 ->
      return $ map fromIntegral $ c'netcode_address_t'data'ipv4 addr
    AddressMode'IPv6 ->  return $ c'netcode_address_t'data'ipv6 addr
    _                ->  return []

-- | Returns an address with the given values interpreted using the given mode.
-- For IPv4 addresses, only the bottom 8 bits of each 16-bit word will be used.
-- The list will be zero padded to contain enough values to fill the address as
-- needed.
constructAddress :: AddressMode
                 -> Word16       -- ^ Port
                 -> [Word16]
                 -> IO Address
constructAddress mode port vals =
  let ipv4Vals = take 4 $ map fromIntegral $ vals <> repeat 0
      ipv6Vals = take 8 $ vals <> repeat 0
   in do
    address <- mallocForeignPtr
    withForeignPtr address $ \ptr -> do
      addrVal <- peek ptr
      case mode of
        AddressMode'IPv4 -> poke ptr $ addrVal {
            c'netcode_address_t'data'ipv4 = ipv4Vals
          , c'netcode_address_t'port = port
          , c'netcode_address_t'type = c'NETCODE_ADDRESS_IPV4
          }
        AddressMode'IPv6 -> poke ptr $ addrVal {
            c'netcode_address_t'data'ipv6 = ipv6Vals
          , c'netcode_address_t'port = port
          , c'netcode_address_t'type = c'NETCODE_ADDRESS_IPV6
          }
        _ -> fail "Cannot construct address with unknown address mode"
    return $ Address address

-- | Takes a 'String' and parses it to create an 'Address'. The string should
-- be formatted as either a valid IPv4 or IPv6 address. It does not, however,
-- support dual address modes.
parseAddress :: String -> IO Address
parseAddress addrStr = do
    address <- mallocForeignPtr
    retVal <- withForeignPtr address $ \addressPtr ->
                withCString addrStr (`c'netcode_parse_address` addressPtr)
    unless (retVal == c'NETCODE_OK) $ fail "Unable to parse address"
    return $ Address address

-- | Returns a string that represents the given 'Address'.
addressToString :: Address -> IO String
addressToString (Address addrPtr) =
    let maxAddrStringLen = 256
     in allocaBytes maxAddrStringLen $ \addrStr -> do
         _ <- withForeignPtr addrPtr (`c'netcode_address_to_string` addrStr)
         peekCString addrStr

-- | Returns True if two addresses are equal by examining their memory contents.
addressEqual :: Address -> Address -> IO Bool
addressEqual (Address addrPtrA) (Address addrPtrB) =
    withForeignPtr addrPtrA $ \ptrA ->
        withForeignPtr addrPtrB $ \ptrB ->
            (/= 0) <$> c'netcode_address_equal ptrA ptrB

