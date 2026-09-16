-- | The POSIX calls the handle layer reaches for.
--
-- GHC's module of this name binds most of @unistd.h@. Only what the handle
-- layer needs is here.
module System.Posix.Internals
  ( FD,
    fdGetMode,
  )
where

import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import Foreign.C.Error (Errno (..), eINVAL, errnoToIOError)
import Foreign.C.Types (CInt)
import GHC.Base (Monad (..))
import GHC.IO (IO)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.Runtime (decodeError, descriptorMode)
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (ioError)
import GHC.Real (fromIntegral)

-- | A file descriptor, as the C calls number them.
type FD = CInt

-- | The mode a descriptor was opened with.
--
-- A descriptor the program already has says how it may be used, and that is
-- what decides whether a 'GHC.IO.Handle.Types.Handle' over it reads, writes
-- or does both. The host that has no descriptors to report a mode for raises
-- the unsupported-operation error.
fdGetMode :: FD -> IO IOMode
fdGetMode descriptor = do
  mode <- descriptorMode (fromIntegral descriptor)
  case mode < 0 of
    True -> failWith (Errno (fromIntegral (decodeError mode)))
    False -> case ioModeOfNumber mode of
      Just ioMode -> return ioMode
      Nothing -> failWith eINVAL
  where
    failWith errno = ioError (errnoToIOError "fdGetMode" errno Nothing Nothing)

-- | The 'IOMode' an open request numbers this way.
ioModeOfNumber :: Int -> Maybe IOMode
ioModeOfNumber number =
  case number == 0 of
    True -> Just ReadMode
    False -> case number == 1 of
      True -> Just WriteMode
      False -> case number == 2 of
        True -> Just AppendMode
        False -> case number == 3 of
          True -> Just ReadWriteMode
          False -> Nothing
