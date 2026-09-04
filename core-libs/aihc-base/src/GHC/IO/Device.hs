-- | Device classes. The definitions live in "GHC.Internal.IO.Types".
module GHC.IO.Device
  ( RawIO (..),
    IODevice (..),
    IODeviceType (..),
    SeekMode (..),
  )
where

import GHC.Internal.IO.Types (IODevice (..), IODeviceType (..), RawIO (..), SeekMode (..))
