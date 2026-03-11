module TimerWheel.Internal.Prelude
  ( Seconds,
    module Reexport,
  )
where

import Control.Monad as Reexport (when)
import Data.Coerce as Reexport (coerce)
import Data.Fixed (E9, Fixed)
import Data.Functor as Reexport (void)
import Data.IORef as Reexport (IORef, newIORef, readIORef, writeIORef)
import Data.Word as Reexport (Word64)
import GHC.Generics as Reexport (Generic)
import Prelude as Reexport hiding (lookup, null)

-- | A number of seconds, with nanosecond precision.
--
-- You can use numeric literals to construct a value of this type, e.g. @0.5@.
--
-- Otherwise, to convert from a type like @Int@ or @Double@, you can use the generic numeric conversion function
-- @realToFrac@.
type Seconds =
  Fixed E9
