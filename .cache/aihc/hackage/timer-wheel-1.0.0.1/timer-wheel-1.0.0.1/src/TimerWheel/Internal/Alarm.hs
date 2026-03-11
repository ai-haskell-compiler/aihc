module TimerWheel.Internal.Alarm
  ( Alarm (..),
  )
where

import TimerWheel.Internal.Nanoseconds (Nanoseconds)
import TimerWheel.Internal.Prelude

data Alarm
  = OneShot !(IO ())
  | Recurring !(IO ()) {-# UNPACK #-} !Nanoseconds !(IORef Bool)
  | Recurring_ !(IO ()) {-# UNPACK #-} !Nanoseconds
