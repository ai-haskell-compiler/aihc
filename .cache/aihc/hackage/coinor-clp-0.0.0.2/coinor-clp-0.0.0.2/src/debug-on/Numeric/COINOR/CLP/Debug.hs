module Numeric.COINOR.CLP.Debug where

import qualified Numeric.COINOR.CLP.FFI as FFI
import Foreign.Ptr (Ptr)


initLog :: Ptr FFI.Simplex -> IO ()
initLog lp = FFI.setLogLevel lp 1
