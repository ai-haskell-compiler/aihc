-- | The I/O subsystem of the program.
--
-- Only Windows has a second I/O subsystem. Each target of aihc is a POSIX
-- target, thus this module is the POSIX branch of the GHC module.
module GHC.IO.SubSystem
  ( withIoSubSystem,
    withIoSubSystem',
    whenIoSubSystem,
    ioSubSystem,
    IoSubSystem (..),
    conditional,
    (<!>),
    isWindowsNativeIO,
  )
where

import Control.Monad (when)
import Prelude

infixl 7 <!>

-- | The I/O subsystem to use in the program.
data IoSubSystem
  = -- | Use a POSIX I/O subsystem.
    IoPOSIX
  | -- | Use the native I/O subsystem of the platform. On a POSIX system, this
    -- is the same as 'IoPOSIX'.
    IoNative
  deriving (Eq, Enum, Show)

-- | Select a value for the I/O subsystem. On a POSIX system, this always
-- gives the first value.
conditional :: a -> a -> a
conditional posix _ = posix

-- | The operator form of 'conditional'.
(<!>) :: a -> a -> a
(<!>) = conditional

-- | Whether the program uses the native Windows I/O subsystem.
isWindowsNativeIO :: Bool
isWindowsNativeIO = False <!> True

-- | The I/O subsystem in use. On a POSIX system, this is always 'IoPOSIX'.
ioSubSystem :: IoSubSystem
ioSubSystem = IoPOSIX

-- | Give the I/O subsystem in use to an action.
withIoSubSystem :: (IoSubSystem -> IO a) -> IO a
withIoSubSystem f = f ioSubSystem

-- | Give the I/O subsystem in use to a function.
withIoSubSystem' :: (IoSubSystem -> a) -> a
withIoSubSystem' f = f ioSubSystem

-- | Run the action only if the given I/O subsystem is in use.
whenIoSubSystem :: IoSubSystem -> IO () -> IO ()
whenIoSubSystem subsystem action = when (ioSubSystem == subsystem) action
