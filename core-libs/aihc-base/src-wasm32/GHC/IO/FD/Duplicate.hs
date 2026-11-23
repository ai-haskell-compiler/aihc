-- | WASI has no call that gives a second descriptor for an open file, so
-- no descriptor has a duplicate.
module GHC.IO.FD.Duplicate
  ( descriptorDup,
    descriptorDup2,
  )
where

import Foreign.C.Types (CInt)
import GHC.IO (IO)
import GHC.Internal.Base (String)
import GHC.Internal.IO.Types (ioe_unsupportedOperation)

descriptorDup :: String -> CInt -> IO CInt
descriptorDup _ _ = ioe_unsupportedOperation

descriptorDup2 :: String -> CInt -> CInt -> IO ()
descriptorDup2 _ _ _ = ioe_unsupportedOperation
