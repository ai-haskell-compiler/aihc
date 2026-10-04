-- | WASI streams have no position. The runtime reaches WASI through the
-- preview 3 bindings only, and @lseek@ of the C library needs preview 1, so
-- no descriptor can seek.
module GHC.IO.FD.Position
  ( descriptorSeekable,
    descriptorSeek,
    descriptorSize,
  )
where

import Data.Bool (Bool (..))
import Foreign.C.Types (CInt)
import GHC.Base (Monad (..), String)
import GHC.IO (IO)
import GHC.Integer (Integer)
import GHC.Internal.IO.Types (SeekMode, ioe_unsupportedOperation)

descriptorSeekable :: CInt -> IO Bool
descriptorSeekable _ = return False

descriptorSeek :: String -> CInt -> SeekMode -> Integer -> IO Integer
descriptorSeek _ _ _ _ = ioe_unsupportedOperation

descriptorSize :: String -> CInt -> IO Integer
descriptorSize _ _ = ioe_unsupportedOperation
